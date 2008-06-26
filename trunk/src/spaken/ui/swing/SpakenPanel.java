/* Created on Jun 26, 2008. */
package spaken.ui.swing;

import java.awt.BorderLayout;
import java.awt.Component;

import javax.swing.JPanel;
import javax.swing.JToolBar;

import spaken.ui.swing.actions.SetToolAction;

/**
 * @author Martijn van Steenbergen
 */
public class SpakenPanel extends JPanel {

	private SpaceCanvas canvas;

	public SpakenPanel() {
		createGUI();
	}

	private void createGUI() {
		canvas = new SpaceCanvas();

		setLayout(new BorderLayout());
		add(canvas, BorderLayout.CENTER);
		add(createToolbar(), BorderLayout.NORTH);
	}

	private Component createToolbar() {
		JToolBar bar = new JToolBar();
		bar.add(new SetToolAction(canvas, new PointMoveTool()));
		bar.add(new SetToolAction(canvas, new PointCreateTool()));
		return bar;
	}

}
