package org.rstudio.studio.client.workbench.views.ai;
 
import org.rstudio.core.client.widget.RStudioFrame;
import org.rstudio.studio.client.common.AutoGlassPanel;
import org.rstudio.studio.client.workbench.ui.WorkbenchPane;
import org.rstudio.studio.client.server.ServerRequestCallback;
import org.rstudio.studio.client.server.ServerError;
import org.rstudio.studio.client.RStudioGinjector;
import org.rstudio.studio.client.common.GlobalDisplay;

import com.google.gwt.user.client.ui.Widget;
import com.google.inject.Inject;
import com.google.gwt.event.dom.client.KeyCodes;
import com.google.gwt.event.dom.client.KeyDownEvent;
import com.google.gwt.event.dom.client.KeyDownHandler;
import com.google.gwt.user.client.ui.TextBox;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.TextArea;
import com.google.gwt.http.client.Request;
import com.google.gwt.http.client.RequestBuilder;
import com.google.gwt.http.client.RequestCallback;
import com.google.gwt.http.client.RequestException;
import com.google.gwt.http.client.Response;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONParser;
import com.google.gwt.json.client.JSONValue;
import com.google.gwt.json.client.JSONArray;
 
public class AIPane
      extends WorkbenchPane
      implements AIPresenter.Display
{
   @Inject
   protected AIPane()
   {
      super("AI");
      ensureWidget();
   }
 
   @Override
   protected Widget createMainWidget()
   {
      frame_ = new RStudioFrame("AI Pane");
      frame_.setSize("100%", "100%");
      frame_.addStyleName("ace_editor_theme");
      frame_.setUrl("about:blank");

      VerticalPanel panel = new VerticalPanel();
      panel.setWidth("100%");

      inputBox = new TextBox();
      inputBox.setWidth("100%");
      inputBox.getElement().setPropertyString("placeholder", "Search Wikipedia");

      resultArea = new TextArea();
      resultArea.setWidth("100%");
      resultArea.setVisibleLines(10);
      resultArea.setReadOnly(true);

      inputBox.addKeyDownHandler(new KeyDownHandler() {
         @Override
         public void onKeyDown(KeyDownEvent event) {
            if (event.getNativeKeyCode() == KeyCodes.KEY_ENTER) {
               String inputText = inputBox.getText();
               searchWikipedia(inputText);
               inputBox.setText(""); // Clear the input box after submission
            }
         }
      });

      panel.add(inputBox);
      panel.add(resultArea);
      panel.add(frame_);
      
      return new AutoGlassPanel(panel);
   }

   private void searchWikipedia(String query) {
      // First, search for articles matching the query
      String searchUrl = "https://en.wikipedia.org/w/api.php?" +
                        "action=query&" +
                        "format=json&" +
                        "list=search&" +
                        "srsearch=" + query + "&" +
                        "origin=*";
      
      RequestBuilder builder = new RequestBuilder(RequestBuilder.GET, searchUrl);
      
      try {
         builder.sendRequest(null, new RequestCallback() {
            @Override
            public void onResponseReceived(Request request, Response response) {
               if (response.getStatusCode() == 200) {
                  JSONValue jsonValue = JSONParser.parseStrict(response.getText());
                  JSONObject jsonObject = jsonValue.isObject();
                  
                  if (jsonObject != null) {
                     JSONObject json_query = jsonObject.get("query").isObject();
                     if (json_query != null) {
                        JSONArray search = json_query.get("search").isArray();
                        if (search != null && search.size() > 0) {
                           // Get the first result's page ID
                           JSONObject firstResult = search.get(0).isObject();
                           if (firstResult != null) {
                              String pageId = firstResult.get("pageid").isNumber().toString();
                              getArticleSummary(pageId);
                           }
                        } else {
                           resultArea.setText("No results found for: " + query);
                        }
                     }
                  }
               } else {
                  resultArea.setText("Error: " + response.getStatusText());
               }
            }

            @Override
            public void onError(Request request, Throwable exception) {
               resultArea.setText("Error performing search: " + exception.getMessage());
            }
         });
      } catch (RequestException e) {
         resultArea.setText("Error: " + e.getMessage());
      }
   }

   private void getArticleSummary(String pageId) {
      // Get the article summary
      String summaryUrl = "https://en.wikipedia.org/w/api.php?" +
                         "action=query&" +
                         "format=json&" +
                         "prop=extracts&" +
                         "exintro=1&" +
                         "explaintext=1&" +
                         "pageids=" + pageId + "&" +
                         "origin=*";
      
      RequestBuilder builder = new RequestBuilder(RequestBuilder.GET, summaryUrl);
      
      try {
         builder.sendRequest(null, new RequestCallback() {
            @Override
            public void onResponseReceived(Request request, Response response) {
               if (response.getStatusCode() == 200) {
                  JSONValue jsonValue = JSONParser.parseStrict(response.getText());
                  JSONObject jsonObject = jsonValue.isObject();
                  
                  if (jsonObject != null) {
                     JSONObject query = jsonObject.get("query").isObject();
                     if (query != null) {
                        JSONObject pages = query.get("pages").isObject();
                        if (pages != null) {
                           JSONObject page = pages.get(pageId).isObject();
                           if (page != null) {
                              String title = page.get("title").isString().stringValue();
                              String extract = page.get("extract").isString().stringValue();
                              
                              // Truncate the extract if it's too long
                              if (extract.length() > 500) {
                                 extract = extract.substring(0, 500) + "...";
                              }
                              
                              resultArea.setText("Title: " + title + "\n\n" +
                                               "Summary: " + extract + "\n\n" +
                                               "Read more: https://en.wikipedia.org/?curid=" + pageId);
                           }
                        }
                     }
                  }
               } else {
                  resultArea.setText("Error: " + response.getStatusText());
               }
            }

            @Override
            public void onError(Request request, Throwable exception) {
               resultArea.setText("Error getting article: " + exception.getMessage());
            }
         });
      } catch (RequestException e) {
         resultArea.setText("Error: " + e.getMessage());
      }
   }
    
   private TextBox inputBox;
   private TextArea resultArea;
   private RStudioFrame frame_;
}