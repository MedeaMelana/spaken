/* Created on Jun 26, 2008. */
package spaken.ui.swing;

import javax.swing.JFrame;

/**
 * @author Martijn van Steenbergen
 */
public class SpakenApp {

	public static void main(String[] args) {
		JFrame f = new JFrame("Spaken");
		f.setContentPane(new SpakenPanel());
		f.setSize(800, 600);
		f.setVisible(true);
	}

}
