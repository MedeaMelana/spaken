/* Created on Jun 26, 2008. */
package spaken.ui.swing;

import java.awt.*;
import java.util.*;
import java.util.List;

import javax.swing.*;

import spaken.model.*;
import spaken.model.commands.ClearCanvasCommand;
import spaken.model.elements.*;
import spaken.model.elements.intersections.Intersections;
import spaken.ui.swing.actions.*;
import spaken.ui.swing.tools.*;

public class SpakenPanel extends JPanel {

	private SpaceCanvas canvas;

	private JSplitPane splitter;

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
		splitter = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, canvas,
				new JScrollPane(history,
						JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
						JScrollPane.HORIZONTAL_SCROLLBAR_NEVER));
		splitter.setContinuousLayout(true);
		splitter.setOneTouchExpandable(true);
		splitter.setResizeWeight(1);

		// Add splitter and tools.
		setLayout(new BorderLayout());
		add(splitter, BorderLayout.CENTER);
		add(createToolbar(), BorderLayout.NORTH);
	}

	@Override
	protected void paintComponent(Graphics g) {
		if (splitter != null) {
			splitter.setDividerLocation(getWidth() - 200);
			splitter = null;
		}
		super.paintComponent(g);
	}

	private Component createToolbar() {
		JToolBar bar = new JToolBar();
		ButtonGroup bg = new ButtonGroup();
		for (Action a : createToolbarActions()) {
			AbstractButton b = new JToggleButton(a);
			bg.add(b);
			bar.add(b);
		}
		bg.getElements().nextElement().doClick();
		return bar;
	}
	
	private List<Action> createToolbarActions() {
		List<Action> actions = new LinkedList<Action>();
		
		for (Tool tool : createTools()) {
			Action a = new SetToolAction(canvas, tool);
			actions.add(a);
		}
		
		return actions;
	}

	private List<Tool> createTools() {
		List<Tool> tools = new LinkedList<Tool>();
		tools.add(new SelectTool());
		tools.add(new PointCreateTool());
		tools.add(new CreateLineTool());
		tools.add(new CreateCircleTool());
		tools.add(new IntersectionTool());
		
		// hack two theorems together:
		AssumedPoint p1 = new AssumedPoint(0);
		AssumedPoint p2 = new AssumedPoint(1);
		Circle c = new Circle(p1, p1, p2);
		tools.add(new ApplyTheoremTool("circle", new Theorem(c)));
		
		Circle c2 = new Circle(p2, p1, p2);
		spaken.model.elements.Point[] ps = Intersections.intersect(c, c2);
		List<Element<?>> goals = new ArrayList<Element<?>>(3);
		Collections.addAll(goals, ps);
		goals.add(new Line(ps[0], ps[1]));
		tools.add(new ApplyTheoremTool("middelloodlijn", new Theorem(goals)));
		
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
		//file.add(new HackyLoadAction(canvas));
		//file.add(new HackySaveAction(canvas));
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
