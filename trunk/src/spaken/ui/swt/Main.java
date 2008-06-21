package spaken.ui.swt;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;

public class Main {

  public static void main(final String[] args) {
    Display display = new Display();

    final Shell shell = new Shell(display);
    shell.setLayout(new FillLayout());
    Composite composite = new Composite(shell, SWT.EMBEDDED);
    
    shell.open();
    while(!shell.isDisposed()) {
	    if (!display.readAndDispatch()) display.sleep();
    }

    display.dispose ();
  }

} 
