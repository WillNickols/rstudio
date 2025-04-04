package org.rstudio.studio.client.workbench.views.ai;
 
 import org.rstudio.studio.client.workbench.ui.DelayLoadTabShim;
 import org.rstudio.studio.client.workbench.ui.DelayLoadWorkbenchTab;
 
 import com.google.inject.Inject;
 
 public class AITab extends DelayLoadWorkbenchTab<AIPresenter>
 {
    public abstract static class Shim 
         extends DelayLoadTabShim<AIPresenter, AITab>
    {
    }
 
    @Inject
    public AITab(Shim shim)
    {
       super("AI", shim);
       shim_ = shim;
    }
    
    
    @SuppressWarnings("unused")
    private final Shim shim_;
 }