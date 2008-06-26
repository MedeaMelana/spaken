package spaken.ui.swing.actions;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;

import spaken.ui.swing.SpaceCanvas;
import spaken.ui.swing.Tool;

public class SetToolAction extends AbstractAction {

	private SpaceCanvas canvas;
	private Tool tool;

	public SetToolAction(SpaceCanvas canvas, Tool tool) {
		super(tool.getName());
		this.canvas = canvas;
		this.tool = tool;
	}

	public void actionPerformed(ActionEvent e) {
		canvas.setTool(tool);
	}

}
