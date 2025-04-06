/*
 * Ai.java
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

import com.google.gwt.event.logical.shared.HasSelectionHandlers;
import com.google.gwt.event.logical.shared.SelectionEvent;
import com.google.gwt.event.logical.shared.SelectionHandler;
import com.google.inject.Inject;

import org.rstudio.core.client.CsvReader;
import org.rstudio.core.client.CsvWriter;
import org.rstudio.core.client.StringUtil;
import org.rstudio.core.client.command.CommandBinder;
import org.rstudio.core.client.command.Handler;
import org.rstudio.studio.client.application.events.EventBus;
import org.rstudio.studio.client.common.GlobalDisplay;
import org.rstudio.studio.client.workbench.WorkbenchList;
import org.rstudio.studio.client.workbench.WorkbenchListManager;
import org.rstudio.studio.client.workbench.WorkbenchView;
import org.rstudio.studio.client.workbench.commands.Commands;
import org.rstudio.studio.client.workbench.events.ActivatePaneEvent;
import org.rstudio.studio.client.workbench.model.Session;
import org.rstudio.studio.client.workbench.ui.PaneManager;
import org.rstudio.studio.client.workbench.views.BasePresenter;
import org.rstudio.studio.client.workbench.views.ai.events.ActivateAiEvent;
import org.rstudio.studio.client.workbench.views.ai.events.HasAiNavigateHandlers;
import org.rstudio.studio.client.workbench.views.ai.events.ShowAiEvent;
import org.rstudio.studio.client.workbench.views.ai.model.AiServerOperations;
import org.rstudio.studio.client.workbench.views.ai.model.Link;

import java.util.ArrayList;
import java.util.Iterator;

public class Ai extends BasePresenter implements ShowAiEvent.Handler
{
   public interface Binder extends CommandBinder<Commands, Ai> {}

   public interface Display extends WorkbenchView,
      HasAiNavigateHandlers
   {
      String getUrl();
      String getDocTitle();
      void showAi(String aiURL);
      void back();
      void forward();
      void print();
      void popout();
      void refresh();
      void focus();
      void focusSearchAi();

      LinkMenu getHistory();

      /**
       * Returns true if this Ai pane has ever been navigated.
       */
      boolean navigated();
   }

   public interface LinkMenu extends HasSelectionHandlers<String>
   {
      void addLink(Link link);
      void removeLink(Link link);
      boolean containsLink(Link link);
      void clearLinks();
      ArrayList<Link> getLinks();
   }

   @Inject
   public Ai(Display view,
               GlobalDisplay globalDisplay,
               AiServerOperations server,
               WorkbenchListManager listManager,
               Commands commands,
               Binder binder,
               final Session session,
               final EventBus events)
   {
      super(view);
      server_ = server;
      aiHistoryList_ = listManager.getAiHistoryList();
      view_ = view;
      globalDisplay_ = globalDisplay;
      events_ = events;

      binder.bind(commands, this);

      view_.addAiNavigateHandler(aiNavigateEvent ->
      {
         if (!historyInitialized_)
            return;

         CsvWriter csvWriter = new CsvWriter();
         csvWriter.writeValue(getApplicationRelativeAiUrl(aiNavigateEvent.getUrl()));
         csvWriter.writeValue(aiNavigateEvent.getTitle());
         aiHistoryList_.append(csvWriter.getValue());

      });
      SelectionHandler<String> navigator = new SelectionHandler<String>() {
         public void onSelection(SelectionEvent<String> selectionEvent)
         {
            showAi(selectionEvent.getSelectedItem());
         }
      };
      view_.getHistory().addSelectionHandler(navigator);

      // initialize ai history
      aiHistoryList_.addListChangedHandler(listChangedEvent ->
      {
         // clear existing
         final LinkMenu history = view_.getHistory();
         history.clearLinks();

         // initialize from the list
         ArrayList<String> list = listChangedEvent.getList();
         for (String s : list)
         {
            // parse the two fields out
            CsvReader csvReader = new CsvReader(s);
            Iterator<String[]> it = csvReader.iterator();
            if (!it.hasNext())
               continue;
            String[] fields = it.next();
            if (fields.length != 2)
               continue;

            // add the link
            Link link = new Link(fields[0], fields[1]);
            history.addLink(link);
         }

         // one time init
         if (!historyInitialized_)
         {
            // mark us initialized
            historyInitialized_ = true;

            if (session.getSessionInfo().getShowAiHome())
            {
               home();
            }
            else if (!view_.navigated())
            {
               ArrayList<Link> links = history.getLinks();
               if (links.size() > 0)
                  showAi(links.get(0).getUrl());
               else
                  home();
            }
         }
      });

   }

   // Commands handled by Shim for activation from main menu context
   public void onAiHome() { bringToFront(); home(); }
   public void onAiSearch() { bringToFront(); view_.focusSearchAi(); }
   public void onAiBack() { bringToFront(); view_.back(); }
   public void onAiForward() { bringToFront(); view_.forward(); }

   @Handler public void onPrintAi() { view_.print(); }
   @Handler public void onAiPopout() { view_.popout(); }
   @Handler public void onRefreshAi() { view_.refresh(); }
   @Handler
   public void onClearAiHistory()
   {
      if (!historyInitialized_)
         return;

      aiHistoryList_.clear();
   }

   public void onShowAi(ShowAiEvent event)
   {
      showAi(event.getTopicUrl());
      bringToFront();
   }

   public void onActivateAi(ActivateAiEvent event)
   {
      bringToFront();
      view_.focus();
   }

   public void bringToFront()
   {
      events_.fireEvent(new ActivatePaneEvent(PaneManager.AI_PANE));
   }

   private void home()
   {
      showAi("ai/doc/home/");
   }

   public Display getDisplay()
   {
      return view_;
   }

   private void showAi(String topicUrl)
   {
      view_.showAi(server_.getApplicationURL(topicUrl));
   }

   private String getApplicationRelativeAiUrl(String aiUrl)
   {
      String appUrl = server_.getApplicationURL("");
      if (aiUrl.startsWith(appUrl) && !aiUrl.equals(appUrl))
         return StringUtil.substring(aiUrl, appUrl.length());
      else
         return aiUrl;
   }

   void onOpenRStudioIDECheatSheet()
   {
      openCheatSheet("ide_cheat_sheet");
   }

   void onOpenDataImportCheatSheet()
   {
      openCheatSheet("data_import_cheat_sheet");
   }

   void onOpenDataVisualizationCheatSheet()
   {
      openCheatSheet("data_visualization_cheat_sheet");
   }

   void onOpenPackageDevelopmentCheatSheet()
   {
      openCheatSheet("package_development_cheat_sheet");
   }

   void onOpenDataTransformationCheatSheet()
   {
      openCheatSheet("data_transformation_cheat_sheet");
   }

   void onOpenDataWranglingCheatSheet()
   {
      openCheatSheet("data_wrangling_cheat_sheet");
   }

   void onOpenRMarkdownCheatSheet()
   {
      openCheatSheet("r_markdown_cheat_sheet");
   }

   void onOpenRMarkdownReferenceGuide()
   {
      openCheatSheet("r_markdown_reference_guide");
   }

   void onOpenShinyCheatSheet()
   {
      openCheatSheet("shiny_cheat_sheet");
   }

   void onOpenSparklyrCheatSheet()
   {
      openCheatSheet("sparklyr_cheat_sheet");
   }

   void onOpenPurrrCheatSheet()
   {
      openCheatSheet("purrr_cheat_sheet");
   }

   void onBrowseCheatSheets()
   {
      globalDisplay_.openWindow("https://www.posit.co/resources/cheatsheets/");
   }

   private void openCheatSheet(String name)
   {
      globalDisplay_.openRStudioLink(name, false);
   }

   void onOpenRoxygenQuickReference()
   {
      events_.fireEvent(new ShowAiEvent("ai/doc/roxygen_ai.html"));
   }

   void onDebugAi()
   {
      globalDisplay_.openRStudioLink("visual_debugger");
   }

   void onMarkdownAi()
   {
      events_.fireEvent(new ShowAiEvent("ai/doc/markdown_ai.html"));
   }

   void onShowAccessibilityAi()
   {
      globalDisplay_.openRStudioLink("rstudio_a11y", false);
   }

   void onProfileAi()
   {
      globalDisplay_.openRStudioLink("profiling_ai", false);
   }

   private Display view_;
   private AiServerOperations server_;
   private WorkbenchList aiHistoryList_;
   private boolean historyInitialized_;
   private GlobalDisplay globalDisplay_;
   private EventBus events_;
}
