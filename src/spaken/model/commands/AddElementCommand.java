package spaken.model.commands;

import spaken.model.Command;
import spaken.model.Element;
import spaken.ui.swing.SpaceCanvas;

public class AddElementCommand implements Command {

	private SpaceCanvas canvas;

	private Element element;

	public AddElementCommand(SpaceCanvas canvas, Element element) {
		this.canvas = canvas;
		this.element = element;
	}

	public void execute() {
		canvas.getSpace().add(element);
		canvas.repaint();
	}

	public void undo() {
		canvas.getSpace().remove(element);
		canvas.repaint();
	}

	public void redo() {
		execute();
	}

	public String getLabel() {
		return "Create " + element.getClass().getSimpleName();
	}

}
