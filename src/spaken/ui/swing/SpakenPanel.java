/* Created on Jun 26, 2008. */
package spaken.ui.swing;

import java.awt.BorderLayout;
import java.awt.Component;
import java.util.LinkedList;
import java.util.List;

import javax.swing.AbstractButton;
import javax.swing.Action;
import javax.swing.ButtonGroup;
import javax.swing.JPanel;
import javax.swing.JToggleButton;
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
		ButtonGroup bg = new ButtonGroup();
		for (Tool tool : createTools()) {
			Action a = new SetToolAction(canvas, tool);
			AbstractButton b = new JToggleButton(a);
			bg.add(b);
			bar.add(b);
		}
		bg.setSelected(bg.getElements().nextElement().getModel(), true);
		return bar;
	}

	private List<Tool> createTools() {
		List<Tool> tools = new LinkedList<Tool>();
		tools.add(new PointMoveTool());
		tools.add(new PointCreateTool());
		return tools;
	}

}
