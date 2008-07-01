/* Created on Jun 26, 2008. */
package spaken.ui.swing;

import java.awt.BorderLayout;
import java.awt.Component;
import java.util.LinkedList;
import java.util.List;

import javax.swing.AbstractButton;
import javax.swing.Action;
import javax.swing.ButtonGroup;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;

import spaken.model.commands.ClearCanvasCommand;
import spaken.ui.swing.actions.ClearCanvasAction;
import spaken.ui.swing.actions.ExitAction;
import spaken.ui.swing.actions.RedoAction;
import spaken.ui.swing.actions.SetToolAction;
import spaken.ui.swing.actions.UndoAction;

public class SpakenPanel extends JPanel {

	private SpaceCanvas canvas;

	public SpakenPanel() {
		createGUI();
	}

	private void createGUI() {
		// Create canvas.
		canvas = new SpaceCanvas();

		// Create history list.
		CommandListModel model = new CommandListModel(canvas.getHistory());
		canvas.getHistory().addListener(model);
		JList history = new JList(model);
		history.setSelectionModel(new CommandListSelectionModel(canvas
				.getHistory()));

		// Set initial command.
		canvas.getHistory().execute(new ClearCanvasCommand(canvas));

		// Create splitter with canvas and history list.
		final JSplitPane splitter = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
				canvas, history);
		splitter.setDividerLocation(600);
		splitter.setOneTouchExpandable(true);
		splitter.setResizeWeight(1);

		// Add splitter and tools.
		setLayout(new BorderLayout());
		add(splitter, BorderLayout.CENTER);
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
		bg.getElements().nextElement().doClick();
		return bar;
	}

	private List<Tool> createTools() {
		List<Tool> tools = new LinkedList<Tool>();
		tools.add(new PointCreateTool());
		tools.add(new PointMoveTool());
		tools.add(new CreateLineTool());
		tools.add(new CreateCircleTool());
		tools.add(new IntersectionTool());
		return tools;
	}

	public JMenuBar createMenuBar() {
		JMenuBar bar = new JMenuBar();
		bar.add(createFileMenu());
		bar.add(createEditMenu());
		return bar;
	}

	private JMenu createFileMenu() {
		JMenu file = new JMenu("File");
		file.add(new ExitAction());
		return file;
	}

	private JMenu createEditMenu() {
		JMenu edit = new JMenu("Edit");
		edit.add(new UndoAction(canvas.getHistory()));
		edit.add(new RedoAction(canvas.getHistory()));
		edit.addSeparator();
		edit.add(new ClearCanvasAction(canvas));
		return edit;
	}

}
