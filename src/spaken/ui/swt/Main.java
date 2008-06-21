package spaken.ui.swt;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;

public class Main {

  public static void main(final String[] args) {
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

} 
