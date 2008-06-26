/* Created on Jun 26, 2008. */
package spaken.ui.swing;

import java.awt.BorderLayout;

import javax.swing.JPanel;

/**
 * @author Martijn van Steenbergen
 */
public class SpakenPanel extends JPanel {

	public SpakenPanel() {
		createGUI();
	}

	private void createGUI() {
		setLayout(new BorderLayout());
		add(new SpaceCanvas(), BorderLayout.CENTER);
	}

}
