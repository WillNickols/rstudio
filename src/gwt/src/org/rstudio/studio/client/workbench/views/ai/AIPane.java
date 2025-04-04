package org.rstudio.studio.client.workbench.views.ai;
 
 import org.rstudio.core.client.widget.RStudioFrame;
 import org.rstudio.studio.client.common.AutoGlassPanel;
 import org.rstudio.studio.client.workbench.ui.WorkbenchPane;
 
 import com.google.gwt.user.client.ui.Widget;
 import com.google.inject.Inject;
 
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
       return new AutoGlassPanel(frame_);
    }
    
    private RStudioFrame frame_;
 }