package spaken.ui.swt;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.*;
import org.eclipse.swt.events.*;
import org.eclipse.swt.graphics.*;

public class SpakenCanvas extends Canvas {

  public SpakenCanvas(Composite parent) {
    super(parent, 0);
    addPaintListener(new PaintMe());
  }
  
  private class PaintMe implements PaintListener {
    public void paintControl(PaintEvent e) {
      GC g = e.gc;
      g.setAntialias(SWT.ON);
      
      Point size = getSize();
      
      g.setLineCap(SWT.CAP_ROUND);
      g.setLineWidth(1);
      g.drawLine(size.x/4, size.y/4, size.x*3/4, size.y*3/4);
      g.drawLine(0, size.y, size.x, 0);
      
      //g.setLineWidth(4);
      //g.drawLine(size.x/2, size.y/2, size.x/2, size.y/2);
      g.setLineWidth(1);
      g.drawOval(size.x/2 - 2, size.y/2 - 2, 5, 5);
    }
  }
  
} 
