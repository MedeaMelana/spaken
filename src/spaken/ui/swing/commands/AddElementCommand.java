package spaken.ui.swing.commands;

import spaken.model.commands.Command;
import spaken.model.elements.Element;
import spaken.ui.swing.SpaceCanvas;

public class AddElementCommand implements Command {
	/*
	 * TODO Move this out of the spaken.model package, or eliminate the
	 * SpaceCanvas in favour of a Space (and do the repainting through listeners
	 * on Space). I prefer the latter.
	 */

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
