package org.rstudio.studio.client.workbench.views.ai;
 
 import org.rstudio.studio.client.workbench.WorkbenchView;
 import org.rstudio.studio.client.workbench.views.BasePresenter;
 
 import com.google.inject.Inject;
 
 public class AIPresenter
       extends BasePresenter
 {
    public interface Display extends WorkbenchView
    {
    }
    
    @Inject
    protected AIPresenter(Display display)
    {
       super(display);
    }
 
 }