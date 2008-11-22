package spaken.model.commands;

import spaken.model.Command;
import spaken.model.Element;
import spaken.model.elements.Group;
import spaken.ui.swing.SpaceCanvas;

public class AddGroupCommand implements Command {
	/*
	 * TODO Move this out of the spaken.model package, or eliminate the
	 * SpaceCanvas in favour of a Space (and do the repainting through listeners
	 * on Space). I prefer the latter.
	 */

	private SpaceCanvas canvas;

	private Group group;

	public AddGroupCommand(SpaceCanvas canvas, Group group) {
		this.canvas = canvas;
		this.group = group;
	}

	public void execute() {
		for (Element e : group.getElements()) {
			canvas.getSpace().add(e);
		}
		canvas.repaint();
	}

	public void undo() {
		for (Element e : group.getElements()) {
			canvas.getSpace().remove(e);
		}
		canvas.repaint();
	}

	public void redo() {
		execute();
	}

	public String getLabel() {
		return "Create group of " + group.getElements().size() + " elements";
	}

}
