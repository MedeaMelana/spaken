package spaken.ui.swt;

import spaken.model.*;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;

public class Main {

  public static void main(final String[] args) {
    quickTest();
    
    Display display = new Display();

    final Shell shell = new Shell(display);
    shell.setLayout(new FillLayout());
    //Composite inhoud = new Composite(shell, 0);
    
    createToolbox(shell);
    SpakenCanvas canvas = new SpakenCanvas(shell);
    
    shell.open();
    while(!shell.isDisposed()) {
	    if (!display.readAndDispatch()) display.sleep();
    }

    display.dispose ();
  }
  
  
  public static void createToolbox(Composite composite) {
    ToolBar tools = new ToolBar(composite, SWT.VERTICAL);
    
    ToolItem item = new ToolItem(tools, SWT.RADIO);
    item.setText("Passer");
    
    item = new ToolItem(tools, SWT.RADIO);
    item.setText("Potlood");
  }
  
  // Do a little testing
  public static void quickTest() {
    Point p1 = new FixedPoint(0,1);
    Point p2 = new FixedPoint(3,4);
    Point p3 = new FixedPoint(-1,3);
    Point p4 = new FixedPoint(3,1);
    
    Line l1 = new Line(p1,p2);
    Line l2 = new Line(p3,p4);
    
    Point i = Intersections.intersect(l1, l2);
    System.out.println("x=" + i.getX() + ", y=" + i.getY());
  }

} 
