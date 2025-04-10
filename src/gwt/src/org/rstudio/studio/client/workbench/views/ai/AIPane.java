/*
 * AiPane.java
 *
 * Copyright (C) 2022 by Posit Software, PBC
 *
 * Unless you have received this program directly from Posit Software pursuant
 * to the terms of a commercial license agreement with Posit Software, then
 * this program is licensed to you under the terms of version 3 of the
 * GNU Affero General Public License. This program is distributed WITHOUT
 * ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
 * AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) for more details.
 *
 */

package org.rstudio.studio.client.workbench.views.ai;

import org.rstudio.core.client.BrowseCap;
import org.rstudio.core.client.ElementIds;
import org.rstudio.core.client.Point;
import org.rstudio.core.client.Rectangle;
import org.rstudio.core.client.StringUtil;
import org.rstudio.core.client.command.KeyboardShortcut;
import org.rstudio.core.client.command.ShortcutManager;
import org.rstudio.core.client.dom.DomUtils;
import org.rstudio.core.client.dom.ElementEx;
import org.rstudio.core.client.dom.EventProperty;
import org.rstudio.core.client.dom.IFrameElementEx;
import org.rstudio.core.client.dom.WindowEx;
import org.rstudio.core.client.events.NativeKeyDownEvent;
import org.rstudio.core.client.files.FileSystemItem;
import org.rstudio.core.client.hyperlink.AiHyperlinkPopupHeader;
import org.rstudio.core.client.hyperlink.AiPageShower;
import org.rstudio.core.client.hyperlink.AiPreview;
import org.rstudio.core.client.hyperlink.HyperlinkPopupPanel;
import org.rstudio.core.client.hyperlink.HyperlinkPopupPositioner;
import org.rstudio.core.client.regex.Match;
import org.rstudio.core.client.regex.Pattern;
import org.rstudio.core.client.theme.res.ThemeStyles;
import org.rstudio.core.client.widget.CanFocus;
import org.rstudio.core.client.widget.FindTextBox;
import org.rstudio.core.client.widget.FocusHelper;
import org.rstudio.core.client.widget.MessageDialog;
import org.rstudio.core.client.widget.RStudioThemedFrame;
import org.rstudio.core.client.widget.SearchDisplay;
import org.rstudio.core.client.widget.SecondaryToolbar;
import org.rstudio.core.client.widget.SmallButton;
import org.rstudio.core.client.widget.Toolbar;
import org.rstudio.core.client.widget.ToolbarButton;
import org.rstudio.studio.client.RStudioGinjector;
import org.rstudio.studio.client.application.Desktop;
import org.rstudio.studio.client.application.events.EventBus;
import org.rstudio.studio.client.common.AutoGlassPanel;
import org.rstudio.studio.client.common.GlobalDisplay;
import org.rstudio.studio.client.common.GlobalDisplay.NewWindowOptions;
import org.rstudio.studio.client.common.SimpleRequestCallback;
import org.rstudio.studio.client.server.Server;
import org.rstudio.studio.client.workbench.commands.Commands;
import org.rstudio.studio.client.workbench.prefs.model.UserPrefs;
import org.rstudio.studio.client.workbench.ui.WorkbenchPane;
import org.rstudio.studio.client.workbench.views.console.events.SendToConsoleEvent;
import org.rstudio.studio.client.workbench.views.ai.Ai.LinkMenu;
import org.rstudio.studio.client.workbench.views.ai.events.AiNavigateEvent;
import org.rstudio.studio.client.workbench.views.ai.model.VirtualHistory;
import org.rstudio.studio.client.workbench.views.ai.search.AiSearch;

import com.google.gwt.core.client.GWT;
import com.google.gwt.core.client.JavaScriptObject;
import com.google.gwt.core.client.JsArrayString;
import com.google.gwt.core.client.Scheduler;
import com.google.gwt.core.client.Scheduler.RepeatingCommand;
import com.google.gwt.core.client.Scheduler.ScheduledCommand;
import com.google.gwt.dom.client.AnchorElement;
import com.google.gwt.dom.client.Document;
import com.google.gwt.dom.client.Element;
import com.google.gwt.dom.client.EventTarget;
import com.google.gwt.dom.client.NativeEvent;
import com.google.gwt.dom.client.NodeList;
import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.dom.client.KeyCodes;
import com.google.gwt.event.dom.client.KeyDownEvent;
import com.google.gwt.event.dom.client.KeyDownHandler;
import com.google.gwt.event.dom.client.KeyUpEvent;
import com.google.gwt.event.dom.client.KeyUpHandler;
import com.google.gwt.event.dom.client.LoadEvent;
import com.google.gwt.event.dom.client.LoadHandler;
import com.google.gwt.event.logical.shared.ResizeEvent;
import com.google.gwt.event.logical.shared.ResizeHandler;
import com.google.gwt.event.shared.HandlerRegistration;
import com.google.gwt.http.client.URL;
import com.google.gwt.resources.client.ClientBundle;
import com.google.gwt.resources.client.CssResource;
import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.MenuItem;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.google.gwt.user.client.ui.Widget;
import com.google.inject.Inject;
import com.google.inject.Provider;

public class AiPane extends WorkbenchPane
                      implements Ai.Display
{
   @Inject
   public AiPane(Provider<AiSearch> searchProvider,
                   GlobalDisplay globalDisplay,
                   Commands commands,
                   EventBus events,
                   UserPrefs prefs)
   {
      super(constants_.aiText(), events);

      // Set the current instance for JSNI callbacks
      currentInstance_ = this;
      
      searchProvider_ = searchProvider;
      globalDisplay_ = globalDisplay;
      commands_ = commands;
      server_ = RStudioGinjector.INSTANCE.getServer();

      // init with a no-op timer
      popupTimer_ = new Timer()
      {
         @Override
         public void run() {}
      };
      popupCancelled_ = false;

      prefs_ = prefs;

      MenuItem clear = commands.clearAiHistory().createMenuItem(false);
      history_ = new ToolbarLinkMenu(12, true, null, new MenuItem[] { clear });

      Window.addResizeHandler(new ResizeHandler()
      {
         public void onResize(ResizeEvent event)
         {
            history_.getMenu().hide();
         }
      });

      frame_ = new RStudioThemedFrame(
         constants_.aiPaneTitle(),
         null,
         RES.editorStyles().getText(),
         null,
         false,
         true);
      
      frame_.setSize("100%", "100%");
      frame_.setStylePrimaryName("rstudio-AiFrame");
      frame_.addStyleName("ace_editor_theme");
      ElementIds.assignElementId(frame_.getElement(), ElementIds.AI_FRAME);

      navStack_ = new VirtualHistory(frame_);

      // NOTE: we do some pretty strange gymnastics to save the scroll
      // position for the iframe. when the Ai Pane is deactivated
      // (e.g. another tab in the tabset is selected), a synthetic scroll
      // event is sent to the iframe's window, forcing it to scroll back
      // to the top of the window. in order to suppress this behavior, we
      // track whether the scroll event occurred when the tab was deactivated;
      // if it was, then we restore the last-recorded scroll position instead.
      scrollTimer_ = new Timer()
      {
         @Override
         public void run()
         {
            WindowEx contentWindow = getContentWindow();
            if (contentWindow != null)
            {
               if (selected_)
               {
                  scrollPos_ = contentWindow.getScrollPosition();
               }
               else if (scrollPos_ != null)
               {
                  contentWindow.setScrollPosition(scrollPos_);
               }
            }
         }
      };

      prefs_.helpFontSizePoints().bind((Double value) -> refresh());
      
      ensureWidget();
   }

   @Override
   protected Widget createMainWidget()
   {
      // Use a DockLayoutPanel instead of VerticalPanel for better layout control
      com.google.gwt.user.client.ui.DockLayoutPanel mainPanel = new com.google.gwt.user.client.ui.DockLayoutPanel(Unit.PX);
      mainPanel.setSize("100%", "100%");
      
      // Create bottom panel for search widget
      searchWidget_ = searchProvider_.get().getSearchWidget();
      Widget searchWidgetWidget = (Widget)searchWidget_;
      
      // Wrap the search widget in a container with a minimum height
      com.google.gwt.user.client.ui.SimplePanel searchContainer = new com.google.gwt.user.client.ui.SimplePanel();
      searchContainer.setWidget(searchWidgetWidget);
      searchContainer.setStyleName("rstudio-AiSearchContainer");
      searchContainer.getElement().getStyle().setProperty("minHeight", "46px"); // 30px input + 8px padding top/bottom
      searchContainer.getElement().getStyle().setProperty("height", "auto");
      searchContainer.getElement().getStyle().setProperty("display", "block");
      
      // Add the search container to the bottom with flexible height
      mainPanel.addSouth(searchContainer, 46);
      
      // Create a resize handler to adjust the layout when the search widget resizes
      searchWidgetWidget.addHandler(new ResizeHandler() {
         @Override
         public void onResize(ResizeEvent event) {
            // Update container height to match content
            int contentHeight = searchWidgetWidget.getOffsetHeight();
            if (contentHeight > 0) {
               int newHeight = Math.max(46, contentHeight);
               searchContainer.getElement().getStyle().setProperty("height", "auto");
               mainPanel.forceLayout();
            }
         }
      }, ResizeEvent.getType());
      
      // Style the search widget for bottom placement
      searchWidgetWidget.addStyleName(RES.styles().bottomSearchWidget());
      
      // Set placeholder text for the search input
      NodeList<Element> inputs = searchWidgetWidget.getElement().getElementsByTagName("input");
      for (int i = 0; i < inputs.getLength(); i++) {
         Element input = inputs.getItem(i);
         input.setAttribute("placeholder", constants_.searchAiLabel());
         
         // Configure input for multiline support
         configureInputForMultiline(input);
      }
      
      // Force search widget elements to take full width
      Element searchElement = searchWidgetWidget.getElement().getFirstChildElement();
      if (searchElement != null) {
         searchElement.getStyle().setWidth(100, Unit.PCT);
         
         // Find all direct children of the search element and set them to 100% width
         NodeList<Element> children = searchElement.getChildNodes().cast();
         for (int i = 0; i < children.getLength(); i++) {
            if (Element.is(children.getItem(i))) {
               Element child = Element.as(children.getItem(i));
               child.getStyle().setWidth(100, Unit.PCT);
            }
         }
      }
      
      // Main frame takes up the rest of the space
      AutoGlassPanel glassPanel = new AutoGlassPanel(frame_);
      mainPanel.add(glassPanel);
      
      // Schedule a deferred task to ensure proper rendering of the search widget
      Scheduler.get().scheduleDeferred(new ScheduledCommand() {
         @Override
         public void execute() {
            // Get all input elements and make them full width
            NodeList<Element> inputs = searchWidgetWidget.getElement().getElementsByTagName("input");
            for (int i = 0; i < inputs.getLength(); i++) {
               Element input = inputs.getItem(i);
               input.getStyle().setWidth(100, Unit.PCT);
               input.getStyle().setProperty("boxSizing", "border-box");
               input.setAttribute("placeholder", constants_.searchAiLabel());
               
               // Configure input for multiline support
               configureInputForMultiline(input);
            }
            
            // Set full width for all table elements
            NodeList<Element> tables = searchWidgetWidget.getElement().getElementsByTagName("table");
            for (int i = 0; i < tables.getLength(); i++) {
               Element table = tables.getItem(i);
               table.getStyle().setWidth(100, Unit.PCT);
            }
            
            // Find and style the suggest box elements
            NodeList<Element> divs = searchWidgetWidget.getElement().getElementsByTagName("div");
            for (int i = 0; i < divs.getLength(); i++) {
               Element div = divs.getItem(i);
               if (div.getClassName() != null && 
                   div.getClassName().contains("gwt-SuggestBox")) {
                  div.getStyle().setWidth(100, Unit.PCT);
               }
               else if (div.getClassName() != null && 
                       div.getClassName().contains("search")) {
                  div.getStyle().setWidth(100, Unit.PCT);
                  div.getStyle().setProperty("margin-right", "0");
                  // Remove padding that creates the slots/bars
                  div.getStyle().setProperty("padding-left", "0");
                  div.getStyle().setProperty("padding-right", "0");
               }
               else if (div.getClassName() != null && 
                       div.getClassName().contains("searchBoxContainer")) {
                  div.getStyle().setWidth(100, Unit.PCT);
                  // Remove any border or padding that might create slots
                  div.getStyle().setProperty("border-left", "none");
                  div.getStyle().setProperty("border-right", "none");
                  div.getStyle().setProperty("padding-left", "0");
                  div.getStyle().setProperty("padding-right", "0");
               }
            }
            
            // Remove any icon containers or slots
            NodeList<Element> spans = searchWidgetWidget.getElement().getElementsByTagName("span");
            for (int i = 0; i < spans.getLength(); i++) {
               Element span = spans.getItem(i);
               if (span.getClassName() != null && 
                   (span.getClassName().contains("icon") || 
                    span.getClassName().contains("slot"))) {
                  span.getStyle().setProperty("display", "none");
               }
            }
         }
      });
      
      return mainPanel;
   }

   @Override
   public void onBeforeUnselected()
   {
      super.onBeforeUnselected();
      selected_ = false;
   }

   @Override
   public void onSelected()
   {
      super.onSelected();
      selected_ = true;

      if (scrollPos_ == null)
         return;

      IFrameElementEx iframeEl = getIFrameEx();
      if (iframeEl == null)
         return;

      WindowEx windowEl = iframeEl.getContentWindow();
      if (windowEl == null)
         return;

      windowEl.setScrollPosition(scrollPos_);
   }

   @Override
   public void setFocus()
   {
      focus();
   }

   @Override
   public void onResize()
   {
      manageTitleLabelMaxSize();
      
      // Ensure search widget takes up full width when resized
      if (searchWidget_ != null)
      {
         final Widget searchWidgetWidget = (Widget)searchWidget_;
         
         // Style all elements inside search widget to take full width
         Scheduler.get().scheduleDeferred(new ScheduledCommand() {
            @Override
            public void execute() {
               // Get all input elements and ensure they're full width
               NodeList<Element> inputs = searchWidgetWidget.getElement().getElementsByTagName("input");
               for (int i = 0; i < inputs.getLength(); i++) {
                  Element input = inputs.getItem(i);
                  input.getStyle().setWidth(100, Unit.PCT);
                  input.setAttribute("placeholder", constants_.searchAiLabel());
                  
                  // Configure input for multiline support
                  configureInputForMultiline(input);
               }
               
               // Set full width for all table elements
               NodeList<Element> tables = searchWidgetWidget.getElement().getElementsByTagName("table");
               for (int i = 0; i < tables.getLength(); i++) {
                  Element table = tables.getItem(i);
                  table.getStyle().setWidth(100, Unit.PCT);
               }
               
               // Find and style the suggest box elements
               NodeList<Element> divs = searchWidgetWidget.getElement().getElementsByTagName("div");
               for (int i = 0; i < divs.getLength(); i++) {
                  Element div = divs.getItem(i);
                  if (div.getClassName() != null && 
                      div.getClassName().contains("gwt-SuggestBox")) {
                     div.getStyle().setWidth(100, Unit.PCT);
                  }
                  else if (div.getClassName() != null && 
                          div.getClassName().contains("search")) {
                     div.getStyle().setWidth(100, Unit.PCT);
                     div.getStyle().setProperty("margin-right", "0");
                     // Remove padding that creates the slots/bars
                     div.getStyle().setProperty("padding-left", "0");
                     div.getStyle().setProperty("padding-right", "0");
                  }
                  else if (div.getClassName() != null && 
                          div.getClassName().contains("searchBoxContainer")) {
                     div.getStyle().setWidth(100, Unit.PCT);
                     // Remove any border or padding that might create slots
                     div.getStyle().setProperty("border-left", "none");
                     div.getStyle().setProperty("border-right", "none");
                     div.getStyle().setProperty("padding-left", "0");
                     div.getStyle().setProperty("padding-right", "0");
                  }
               }
               
               // Remove any icon containers or slots
               NodeList<Element> spans = searchWidgetWidget.getElement().getElementsByTagName("span");
               for (int i = 0; i < spans.getLength(); i++) {
                  Element span = spans.getItem(i);
                  if (span.getClassName() != null && 
                      (span.getClassName().contains("icon") || 
                       span.getClassName().contains("slot"))) {
                     span.getStyle().setProperty("display", "none");
                  }
               }
            }
         });
      }

      super.onResize();
   }

   private void manageTitleLabelMaxSize()
   {
      if (title_ != null)
      {
         int offsetWidth = getOffsetWidth();
         if (offsetWidth > 0)
         {
            int newWidth = offsetWidth - 25;
            if (newWidth > 0)
               title_.getElement().getStyle().setPropertyPx("maxWidth", newWidth);
         }
      }
   }

   @Override
   protected void onLoad()
   {
      super.onLoad();

      if (!initialized_)
      {
         initialized_ = true;

         initAiCallbacks();

         Scheduler.get().scheduleDeferred(new ScheduledCommand()
         {
            public void execute()
            {
               manageTitleLabelMaxSize();
            }
         });
      }
   }

   public final native void initAiCallbacks() /*-{
      function addEventHandler(subject, eventName, handler) {
         if (subject.addEventListener) {
            subject.addEventListener(eventName, handler, false);
         }
         else {
            subject.attachEvent(eventName, handler);
         }
      }

      var thiz = this;
      
      $wnd.aiNavigated = function(document, win) {
         thiz.@org.rstudio.studio.client.workbench.views.ai.AiPane::aiNavigated(Lcom/google/gwt/dom/client/Document;)(document);
         addEventHandler(win, "unload", function () {
            thiz.@org.rstudio.studio.client.workbench.views.ai.AiPane::unload()();
         });
      };
      
      $wnd.aiNavigate = function(url) {
         if (url.length)
            thiz.@org.rstudio.studio.client.workbench.views.ai.AiPane::showAi(Ljava/lang/String;)(url);
      };

      $wnd.aiKeydown = function(e) {
         thiz.@org.rstudio.studio.client.workbench.views.ai.AiPane::handleKeyDown(Lcom/google/gwt/dom/client/NativeEvent;)(e);
      };
      
      $wnd.aiMousedown = function(e) {
         thiz.@org.rstudio.studio.client.workbench.views.ai.AiPane::handleMouseDown(*)(e);
      };

      $wnd.aiMouseover = function(e) {
         thiz.@org.rstudio.studio.client.workbench.views.ai.AiPane::handleMouseOver(*)(e);
      }

      $wnd.aiMouseout = function(e) {
         thiz.@org.rstudio.studio.client.workbench.views.ai.AiPane::handleMouseOut(*)(e);
      }

      $wnd.aiClick = function(e) {
         thiz.@org.rstudio.studio.client.workbench.views.ai.AiPane::handleClick(*)(e);
      }
      
   }-*/;



   // delegate shortcuts which occur while Ai has focus

   private void handleKeyDown(NativeEvent e)
   {
      // determine whether this key-combination means we should focus find
      int mod = KeyboardShortcut.getModifierValue(e);
      if (mod == (BrowseCap.hasMetaKey() ? KeyboardShortcut.META
                                         : KeyboardShortcut.CTRL))
      {
         if (e.getKeyCode() == 'F')
         {
            e.preventDefault();
            e.stopPropagation();
            WindowEx.get().focus();
            findTextBox_.focus();
            findTextBox_.selectAll();
            return;
         }
         else if (e.getKeyCode() == KeyCodes.KEY_ENTER)
         {
            // extract the selected code, if any
            String code = frame_.getWindow().getSelectedText();
            if (code.isEmpty())
               return;

            // send it to the console
            events_.fireEvent(new SendToConsoleEvent(
                  code,
                  true, // execute
                  false // focus
                  ));
            return;
         }
      }

      // don't let backspace perform browser back
      DomUtils.preventBackspaceCausingBrowserBack(e);

      // ESC closes ai preview popup
      if (e.getKeyCode() == KeyCodes.KEY_ESCAPE)
      {
         if (popup_ != null)
            popup_.hide();
         popup_ = null;
      }
      
      // delegate to the shortcut manager
      NativeKeyDownEvent evt = new NativeKeyDownEvent(e);
      ShortcutManager.INSTANCE.onKeyDown(evt);
      if (evt.isCanceled())
      {
         e.preventDefault();
         e.stopPropagation();

         // since this is a shortcut handled by the main window
         // we set focus to it
         WindowEx.get().focus();
      }
   }
   
   private void handleMouseDown(NativeEvent event)
   {
      // Not required on Electron; back / forward navigation is handled
      // via a top-level Javascript event handler. See DesktopApplicationHeader.java
      // for more details.
      if (BrowseCap.isElectron())
         return;
      
      int button = EventProperty.button(event);
      if (button == EventProperty.MOUSE_BACKWARD)
      {
         event.stopPropagation();
         event.preventDefault();
         commands_.aiBack().execute();
      }
      else if (button == EventProperty.MOUSE_FORWARD)
      {
         event.stopPropagation();
         event.preventDefault();
         commands_.aiForward().execute();
      }
   }

   private void handleMouseOver(NativeEvent event)
   {
      EventTarget target = event.getEventTarget();
      if (AnchorElement.is(target)) 
      {
         AnchorElement anchor = AnchorElement.as(Element.as(target));
         String url = anchor.getHref();
         Match match = AI_PATTERN.match(url, 0);
         if (match != null) 
         {
            // cancel previous timer
            popupTimer_.cancel();

            // hide previous popup immediately
            if (popup_ != null)
               popup_.hide();
            
            // and schedule a new one
            popupTimer_ = new Timer()
            {
               @Override
               public void run()
               {
                  String topic = decodeURIComponent(match.getGroup(2));
                  
                  popup_ = new HyperlinkPopupPanel(new AiPageShower() {

                     @Override
                     public void showAi() {
                        AiPane.this.showAi(url);
                     }
                     
                  });

                  // pkg might not be the actual package
                  // so we need to do the same as what the internal ai system would:
                  server_.followAiTopic(url, new SimpleRequestCallback<JsArrayString>(){

                     @Override
                     public void onResponseReceived(JsArrayString files)
                     {
                        if (files.length() == 1)
                        {
                           String pkg = files.get(0).replaceFirst("/ai/.*$", "").replaceFirst("^.*/", "");
                        
                           final VerticalPanel panel = new VerticalPanel();
                           panel.add(new AiHyperlinkPopupHeader(topic, pkg));
                           panel.add(new AiPreview(topic, pkg, () -> 
                           {
                              popup_.setContent(panel);

                              Rectangle bounds = new Rectangle(event.getClientX() + getIFrameEx().getAbsoluteLeft(), 
                                                               event.getClientY() + getIFrameEx().getAbsoluteTop(), 
                                                               anchor.getClientWidth(), 
                                                               anchor.getClientWidth());
                              HyperlinkPopupPositioner positioner = new HyperlinkPopupPositioner(bounds, popup_);
                                 
                              if (!popupCancelled_) 
                                 popup_.setPopupPositionAndShow(positioner);
                           }));
                        }
                     }
                  });

               }
            };
            popupCancelled_ = false;
            popupTimer_.schedule(400);
         }
      }
   }

   private void handleMouseOut(NativeEvent event) {
      EventTarget target = event.getEventTarget();
      if (AnchorElement.is(target)) 
      {
         // mark the popup as cancelled. This handles the case when the 
         // timer has finished but the popup has not been 
         // calculated yet, i.e. server_.followAiTopic() has not returned
         popupCancelled_ = true;

         // cancel a popup that is scheduled for later
         popupTimer_.cancel();
         
         // then hide popup if necessary
         if (popup_ != null)
            popup_.hide();
      }
   }

   private native String decodeURIComponent(String encoded) /*-{
      return decodeURIComponent(encoded);
   }-*/;

   private void handleClick(NativeEvent event)
   {
      if (popup_ != null)
         popup_.hide();
   }

   private void aiNavigated(Document doc)
   {
      NodeList<Element> elements = doc.getElementsByTagName("a");
      for (int i = 0; i < elements.getLength(); i++)
      {
         ElementEx a = (ElementEx) elements.getItem(i);
         String href = a.getAttribute("href", 2);
         if (href == null)
            continue;

         if (href.contains(":") || href.endsWith(".pdf"))
         {
            // external links
            AnchorElement aElement = a.cast();
            aElement.setTarget("_blank");
         }
         else
         {
            // Internal links need to be handled in JavaScript so that
            // they can participate in virtual session history. This
            // won't have any effect for right-click > Show in New Window
            // but that's a good thing.
            a.setAttribute(
                  "onclick",
                  "window.parent.aiNavigate(this.href); return false");
         }
      }

      String effectiveTitle = getDocTitle(doc);
      title_.setText(effectiveTitle);
      this.fireEvent(new AiNavigateEvent(doc.getURL(), effectiveTitle));
   }

   private String getDocTitle(Document doc)
   {
      String docUrl = StringUtil.notNull(doc.getURL());
      String docTitle = doc.getTitle();

      String previewPrefix = new String("/ai/preview?file=");
      int previewLoc = docUrl.indexOf(previewPrefix);
      if (previewLoc != -1)
      {
         String file = StringUtil.substring(docUrl, previewLoc + previewPrefix.length());
         file = URL.decodeQueryString(file);
         FileSystemItem fsi = FileSystemItem.createFile(file);
         docTitle = fsi.getName();
      }
      else if (StringUtil.isNullOrEmpty(docTitle))
      {
         String url = new String(docUrl);
         url = url.split("\\?")[0];
         url = url.split("#")[0];
         String[] chunks = url.split("/");
         docTitle = chunks[chunks.length - 1];
      }

      return docTitle;
   }

   private void unload()
   {
      title_.setText("");
   }

   @Override
   protected Toolbar createMainToolbar()
   {
      Toolbar toolbar = new Toolbar(constants_.aiTabLabel());

      toolbar.addLeftWidget(commands_.aiBack().createToolbarButton());
      toolbar.addLeftWidget(commands_.aiForward().createToolbarButton());
      toolbar.addLeftWidget(commands_.aiHome().createToolbarButton());
      toolbar.addLeftSeparator();
      if (!Desktop.isDesktop())
      {
         // QtWebEngine doesn't currently support window.print(); see:
         // https://bugreports.qt.io/browse/QTBUG-53745
         toolbar.addLeftWidget(commands_.printAi().createToolbarButton());
         toolbar.addLeftSeparator();
      }
      toolbar.addLeftWidget(commands_.aiPopout().createToolbarButton());

      // Remove search widget from toolbar since it's now at the bottom
      // searchWidget_ = searchProvider_.get().getSearchWidget();
      // toolbar.addRightWidget((Widget)searchWidget_);

      toolbar.addRightSeparator();

      ToolbarButton refreshButton = commands_.refreshAi().createToolbarButton();
      refreshButton.addStyleName(ThemeStyles.INSTANCE.refreshToolbarButton());
      toolbar.addRightWidget(refreshButton);

      return toolbar;
   }

   @Override
   protected SecondaryToolbar createSecondaryToolbar()
   {
      SecondaryToolbar toolbar = new SecondaryToolbar(constants_.aiTabSecondLabel());

      title_ = new Label();
      title_.addStyleName(RES.styles().topicTitle());
      toolbar.addLeftPopupMenu(title_, history_.getMenu());

      ThemeStyles styles = ThemeStyles.INSTANCE;
      toolbar.getWrapper().addStyleName(styles.tallerToolbarWrapper());

      final SmallButton btnNext = new SmallButton("&gt;", true);
      btnNext.getElement().setAttribute("aria-label", constants_.findNextLabel());
      btnNext.setTitle(constants_.findNextLabel());
      btnNext.addStyleName(RES.styles().topicNavigationButton());
      btnNext.setVisible(false);
      btnNext.addClickHandler(new ClickHandler() {
         @Override
         public void onClick(ClickEvent event)
                                           {
                                              findNext();
                                                         }
      });

      final SmallButton btnPrev = new SmallButton("&lt;", true);
      btnPrev.getElement().setAttribute("aria-label", constants_.findPreviousLabel());
      btnPrev.setTitle(constants_.findPreviousLabel());
      btnPrev.addStyleName(RES.styles().topicNavigationButton());
      btnPrev.setVisible(false);
      btnPrev.addClickHandler(new ClickHandler() {
         @Override
         public void onClick(ClickEvent event)
                                           {
                                              findPrev();
                                                         }
      });


      findTextBox_ = new FindTextBox(constants_.findInTopicLabel());
      findTextBox_.addStyleName(RES.styles().findTopicTextbox());
      findTextBox_.setOverrideWidth(90);
      ElementIds.assignElementId(findTextBox_, ElementIds.SW_AI_FIND_IN_TOPIC);
      toolbar.addLeftWidget(findTextBox_);
      findTextBox_.addKeyUpHandler(new KeyUpHandler() {

         @Override
         public void onKeyUp(KeyUpEvent event)
         {
            // ignore modifier key release
            if (event.getNativeKeyCode() == KeyCodes.KEY_CTRL ||
                event.getNativeKeyCode() == KeyCodes.KEY_ALT ||
                event.getNativeKeyCode() == KeyCodes.KEY_SHIFT)
            {
               return;
            }

            WindowEx contentWindow = getContentWindow();
            if (contentWindow != null)
            {
               // escape means exit find mode and put focus
               // into the main content window
               if (event.getNativeKeyCode() == KeyCodes.KEY_ESCAPE)
               {
                  event.preventDefault();
                  event.stopPropagation();
                  clearTerm();
                  contentWindow.focus();
               }
               else
               {
                  // prevent two enter keys in rapid succession from
                  // minimizing or maximizing the ai pane
                  if (event.getNativeKeyCode() == KeyCodes.KEY_ENTER)
                  {
                     event.preventDefault();
                     event.stopPropagation();
                  }

                  // check for term
                  String term = findTextBox_.getValue().trim();

                  int modifier = KeyboardShortcut.getModifierValue(event.getNativeEvent());
                  boolean isShift = modifier == KeyboardShortcut.SHIFT;

                  // if there is a term then search for it
                  if (term.length() > 0)
                  {
                     // make buttons visible
                     setButtonVisibility(true);

                     // perform the find (check for incremental)
                     if (isIncrementalFindSupported())
                     {
                        boolean incremental =
                         !event.isAnyModifierKeyDown() &&
                         (event.getNativeKeyCode() != KeyCodes.KEY_ENTER);

                        performFind(term, !isShift, incremental);
                     }
                     else
                     {
                        if (event.getNativeKeyCode() == KeyCodes.KEY_ENTER)
                           performFind(term, !isShift, false);
                     }
                  }

                  // no term means clear term and remove selection
                  else
                  {
                     if (isIncrementalFindSupported())
                     {
                        clearTerm();
                        contentWindow.removeSelection();
                     }
                  }
               }
            }
         }

         private void clearTerm()
         {
            findTextBox_.setValue("");
            setButtonVisibility(false);
         }

         private void setButtonVisibility(final boolean visible)
         {
            Scheduler.get().scheduleDeferred(new ScheduledCommand() {
               @Override
               public void execute()
               {
                  btnNext.setVisible(visible);
                  btnPrev.setVisible(visible);
               }
            });
         }
      });

      findTextBox_.addKeyDownHandler(new KeyDownHandler() {

         @Override
         public void onKeyDown(KeyDownEvent event)
         {
            // we handle these directly so prevent the browser
            // from handling them
            if (event.getNativeKeyCode() == KeyCodes.KEY_ESCAPE ||
                event.getNativeKeyCode() == KeyCodes.KEY_ENTER)
            {
               event.preventDefault();
               event.stopPropagation();
            }
         }

      });

      if (isIncrementalFindSupported())
      {
         btnPrev.getElement().getStyle().setMarginRight(3, Unit.PX);
         toolbar.addLeftWidget(btnPrev);
         toolbar.addLeftWidget(btnNext);
      }

      return toolbar;
   }

   private String getTerm()
   {
      return findTextBox_.getValue().trim();
   }

   private void findNext()
   {
      String term = getTerm();
      if (term.length() > 0)
         performFind(term, true, false);
   }

   private void findPrev()
   {
      String term = getTerm();
      if (term.length() > 0)
         performFind(term, false, false);
   }

   private void performFind(String term,
                            boolean forwards,
                            boolean incremental)
   {
      WindowEx contentWindow = getContentWindow();
      if (contentWindow == null)
         return;

      // if this is an incremental search then reset the selection first
      if (incremental)
         contentWindow.removeSelection();

      contentWindow.find(term, false, !forwards, true, false);
   }

   // Firefox changes focus during our typeahead search (it must take
   // focus when you set the selection into the iframe) which breaks
   // typeahead entirely. rather than code around this we simply
   // disable it for Firefox
   private boolean isIncrementalFindSupported()
   {
      return !BrowseCap.isFirefox();
   }

   @Override
   public String getUrl()
   {
      String url = null;
      try
      {
         if (getIFrameEx() != null && getIFrameEx().getContentWindow() != null)
            url = getIFrameEx().getContentWindow().getLocationHref();
      }
      catch (Exception e)
      {
         // attempting to get the URL can throw with a DOM security exception if
         // the current URL is on another domain--in this case we'll just want
         // to return null, so eat the exception.
      }
      return url;
   }

   @Override
   public String getDocTitle()
   {
      return getIFrameEx().getContentDocument().getTitle();
   }

   @Override
   public void focusSearchAi()
   {
      if (searchWidget_ != null)
         FocusHelper.setFocusDeferred(searchWidget_);
   }

   @Override
   public void showAi(String url)
   {
      ensureWidget();
      bringToFront();
      navStack_.navigate(url);
      setLocation(url, Point.create(0, 0));
      navigated_ = true;
   }

   private void setLocation(final String url,
                            final Point scrollPos)
   {
      // allow subsequent calls to setLocation to override any previous
      // call (necessary so two consecutive calls like we get during
      // some startup scenarios don't result in the first url displaying
      // rather than the second)
      targetUrl_ = url;

      RepeatingCommand navigateCommand = new RepeatingCommand() {

         @SuppressWarnings("unused")
         private HandlerRegistration handler_ = frame_.addLoadHandler(new LoadHandler()
         {
            @Override
            public void onLoad(LoadEvent event)
            {
               WindowEx contentWindow = getIFrameEx().getContentWindow();
               contentWindow.setScrollPosition(scrollPos);
               setWindowScrollHandler(contentWindow);

               handler_.removeHandler();
               handler_ = null;
            }
         });

         @Override
         public boolean execute()
         {
            if (getIFrameEx() == null)
               return true;

            if (getIFrameEx().getContentWindow() == null)
               return true;

            if (targetUrl_ == getUrl())
            {
               getIFrameEx().getContentWindow().reload();
            }
            else
            {
               frame_.setUrl(targetUrl_);
               replaceFrameUrl(frame_.getIFrame().cast(), targetUrl_);
            }

            return false;
         }
      };

      if (navigateCommand.execute())
      {
         Scheduler.get().scheduleFixedDelay(navigateCommand, 100);
      }
   }

   @Override
   public void refresh()
   {
      String url = getUrl();
      if (url != null)
         setLocation(url, Point.create(0, 0));
   }

   private WindowEx getContentWindow()
   {
      return getIFrameEx() != null ? getIFrameEx().getContentWindow() : null;
   }

   @Override
   public void back()
   {
      VirtualHistory.Data back = navStack_.back();
      if (back != null)
         setLocation(back.getUrl(), back.getScrollPosition());
   }

   @Override
   public void forward()
   {
      VirtualHistory.Data fwd = navStack_.forward();
      if (fwd != null)
         setLocation(fwd.getUrl(), fwd.getScrollPosition());
   }

   @Override
   public void print()
   {
      getContentWindow().focus();
      getContentWindow().print();
   }

   @Override
   public void popout()
   {
      String href = getContentWindow().getLocationHref();
      NewWindowOptions options = new NewWindowOptions();
      options.setName("aipanepopout_" + popoutCount_++);
      globalDisplay_.openWebMinimalWindow(href, false, 0, 0, options);
   }

   @Override
   public void focus()
   {
      WindowEx contentWindow = getContentWindow();
      if (contentWindow != null)
         contentWindow.focus();
   }

   @Override
   public HandlerRegistration addAiNavigateHandler(AiNavigateEvent.Handler handler)
   {
      return addHandler(handler, AiNavigateEvent.TYPE);
   }


   @Override
   public LinkMenu getHistory()
   {
      return history_;
   }

   @Override
   public boolean navigated()
   {
      return navigated_;
   }

   private IFrameElementEx getIFrameEx()
   {
      return frame_.getElement().cast();
   }

   private void findInTopic(String term, CanFocus findInputSource)
   {
      // get content window
      WindowEx contentWindow = getContentWindow();
      if (contentWindow == null)
         return;

      if (!contentWindow.find(term, false, false, true, false))
      {
         globalDisplay_.showMessage(MessageDialog.INFO,
               constants_.findInTopicLabel(),
               constants_.noOccurrencesFoundMessage(),
               findInputSource);
      }
   }

   private final native void replaceFrameUrl(JavaScriptObject frame, String url) /*-{
      frame.contentWindow.setTimeout(function() {
         this.location.replace(url);
      }, 0);
   }-*/;

   private final native void setWindowScrollHandler(WindowEx window)
   /*-{
      var self = this;
      window.onscroll = $entry(function() {
         self.@org.rstudio.studio.client.workbench.views.ai.AiPane::onScroll()();
      });
   }-*/;

   private void onScroll()
   {
      scrollTimer_.schedule(50);
   }

   /**
    * Configures an input element to support multiline text with auto-expanding height
    * by replacing it with a textarea that has similar attributes
    */
   private native void configureInputForMultiline(Element input) /*-{
      // Only convert to textarea if not already converted
      if (!input.getAttribute("converted-to-textarea")) {
         input.setAttribute("converted-to-textarea", "true");
         
         // Create a textarea to replace the input
         var textarea = $doc.createElement("textarea");
         
         // Copy relevant attributes from input to textarea
         textarea.className = input.className;
         textarea.placeholder = input.placeholder;
         textarea.value = input.value;
         textarea.id = input.id;
         textarea.name = input.name;
         
         // Apply specific styles for textarea behavior
         textarea.style.width = "100%";
         textarea.style.height = "auto";
         textarea.style.minHeight = "30px";
         textarea.style.boxSizing = "border-box";
         textarea.style.resize = "none";
         textarea.style.overflow = "hidden";
         textarea.style.border = "none";
         textarea.style.outline = "none";
         textarea.style.padding = "4px 10px";
         textarea.style.display = "block";
         
         // Make sure font matches exactly the input element and rest of the application
         var computedStyle = window.getComputedStyle(input);
         
         // If the input doesn't have explicit font settings, try to get them from parent elements
         var fontFamily = computedStyle.fontFamily;
         var fontSize = computedStyle.fontSize;
         var fontWeight = computedStyle.fontWeight;
         
         if (fontFamily === "" || fontFamily === "inherit") {
            // Try to get font from parent
            var parent = input.parentElement;
            while (parent) {
               var parentStyle = window.getComputedStyle(parent);
               if (parentStyle.fontFamily && parentStyle.fontFamily !== "" && parentStyle.fontFamily !== "inherit") {
                  fontFamily = parentStyle.fontFamily;
                  break;
               }
               parent = parent.parentElement;
            }
         }
         
         // Apply all font properties
         textarea.style.fontFamily = fontFamily;
         textarea.style.fontSize = fontSize;
         textarea.style.fontWeight = fontWeight;
         textarea.style.fontStyle = computedStyle.fontStyle;
         textarea.style.letterSpacing = computedStyle.letterSpacing;
         textarea.style.lineHeight = computedStyle.lineHeight || "1.4";
         textarea.style.color = computedStyle.color;
         textarea.style.backgroundColor = "transparent";
         
         // Replace the input with the textarea
         input.parentNode.replaceChild(textarea, input);
         
         // Focus the textarea if the input was focused
         if (document.activeElement === input) {
            textarea.focus();
         }
         
         // Auto-resize function
         var resizeTextarea = function() {
            textarea.style.height = "auto"; // Reset height to recalculate
            var newHeight = Math.max(30, textarea.scrollHeight);
            textarea.style.height = newHeight + "px";
            
            // Adjust parent containers
            var parent = textarea.parentElement;
            while (parent && parent.classList) {
               if (parent.classList.contains("search") || 
                   parent.classList.contains("rstheme_center") || 
                   parent.classList.contains("searchBoxContainer") || 
                   parent.classList.contains("searchBoxContainer2")) {
                  parent.style.height = "auto";
                  parent.style.minHeight = "30px";
               }
               parent = parent.parentElement;
            }
            
            // Trigger a resize event
            if (typeof $wnd.CustomEvent === 'function') {
               var event = new $wnd.CustomEvent('resize', {bubbles: true});
               textarea.dispatchEvent(event);
            }
         };
         
         // Add event listeners for automatic resizing
         textarea.addEventListener("input", resizeTextarea);
         textarea.addEventListener("change", resizeTextarea);
         textarea.addEventListener("focus", resizeTextarea);
         
         // Set up the global handler for AI search if not already defined
         if (!$wnd.aiSearchHandler) {
            $wnd.aiSearchHandler = $entry(function(value) {
               // Call the Java method to handle the search
               var thiz = @org.rstudio.studio.client.workbench.views.ai.AiPane::getCurrentInstance()();
               if (thiz) {
                  thiz.@org.rstudio.studio.client.workbench.views.ai.AiPane::handleAiSearchFromJS(Ljava/lang/String;)(value);
               }
            });
         }
         
         // Handle key events - modified to make Enter trigger search
         textarea.addEventListener("keydown", function(e) {
            // If Enter key without any modifier keys
            if (e.keyCode === 13 && !e.ctrlKey && !e.altKey && !e.shiftKey && !e.metaKey) {
               // Prevent default (which would add a newline)
               e.preventDefault();
               
               // Call the global handler to trigger the search
               if ($wnd.aiSearchHandler) {
                  $wnd.aiSearchHandler(textarea.value);
               }
            } 
            else if (e.keyCode === 13 && (e.ctrlKey || e.metaKey)) {
               // Allow Ctrl+Enter or Cmd+Enter (macOS) to insert a newline
               // No need to do anything special, let the default behavior happen
            }
         });
         
         // Initial resize
         setTimeout(resizeTextarea, 0);
         
         // Handle window resize
         $wnd.addEventListener("resize", function() {
            setTimeout(resizeTextarea, 0);
         });
         
         // Return the new textarea to allow further operations if needed
         return textarea;
      }
      
      return input; // Return original input if already converted
   }-*/;

   // Static reference to current instance for JSNI callbacks
   private static AiPane currentInstance_;
   
   // Static getter for the current instance
   private static AiPane getCurrentInstance() {
      return currentInstance_;
   }
   
   // Called from JSNI when Enter is pressed in the search textarea
   private void handleAiSearchFromJS(final String searchValue) {
      if (searchValue == null || searchValue.trim().isEmpty()) 
         return;
      
      // Replace all newline characters with spaces
      final String modifiedSearchValue = searchValue.replace("\n", " ");
      
      Scheduler.get().scheduleDeferred(() -> {
         if (searchWidget_ != null) {
               // Fire selection commit event on the search widget using modified search string
               org.rstudio.core.client.events.SelectionCommitEvent.fire(searchWidget_, modifiedSearchValue);
               
               // Clear the text box after triggering the search
               Scheduler.get().scheduleDeferred(() -> {
                  // Clear the search widget's text
                  searchWidget_.setText("");
                  
                  // Also directly clear any textareas that might be in the search widget
                  NodeList<Element> textareas = ((Widget)searchWidget_).getElement().getElementsByTagName("textarea");
                  for (int i = 0; i < textareas.getLength(); i++) {
                     Element textarea = textareas.getItem(i);
                     textarea.setPropertyString("value", "");
                  }
               });
         }
      });
   }

   public interface Styles extends CssResource
   {
      String findTopicTextbox();
      String topicNavigationButton();
      String topicTitle();
      String bottomSearchWidget();
   }

   public interface EditorStyles extends CssResource
   {
   }

   public interface Resources extends ClientBundle
   {
      @Source("AiPane.css")
      Styles styles();

      @Source("AiPaneEditorStyles.css")
      EditorStyles editorStyles();
   }

   private static final Resources RES = GWT.create(Resources.class);
   static { RES.styles().ensureInjected(); }

   private UserPrefs prefs_;

   private final VirtualHistory navStack_;
   private final ToolbarLinkMenu history_;
   private Label title_;
   private RStudioThemedFrame frame_;
   private FindTextBox findTextBox_;
   private final Provider<AiSearch> searchProvider_;
   private GlobalDisplay globalDisplay_;
   private final Commands commands_;
   private boolean navigated_;
   private boolean initialized_;
   private String targetUrl_;
   private Point scrollPos_;
   private Timer scrollTimer_;
   private boolean selected_;
   private static int popoutCount_ = 0;
   private SearchDisplay searchWidget_;
   private static final AiConstants constants_ = GWT.create(AiConstants.class);
   private Server server_;
   HyperlinkPopupPanel popup_;
   Timer popupTimer_;
   boolean popupCancelled_;

   private static final Pattern AI_PATTERN = Pattern.create("^.*/ai/library/([^/]*)/ai/(.*)$", "");
}
