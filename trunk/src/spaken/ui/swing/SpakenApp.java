/* Created on Jun 26, 2008. */
package spaken.ui.swing;

import javax.swing.JFrame;

/**
 * @author Martijn van Steenbergen
 */
public class SpakenApp {

	public static void main(String[] args) {
		JFrame f = new JFrame("Spaken");
		SpakenPanel p = new SpakenPanel();
		f.setContentPane(p);
		f.setSize(800, 600);
		f.setJMenuBar(p.createMenuBar());
		f.setVisible(true);
		f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	}

}
