package spaken.model.commands;

import java.util.LinkedList;
import java.util.List;

import spaken.model.Command;
import spaken.model.Element;
import spaken.ui.swing.SpaceCanvas;

public class ClearCanvasCommand implements Command {
	/*
	 * TODO Move this out of the spaken.model package, or eliminate the
	 * SpaceCanvas in favour of a Space (and do the repainting through listeners
	 * on Space). I prefer the latter.
	 */
	
	private SpaceCanvas canvas;

	private List<Element> elements = new LinkedList<Element>();

	public ClearCanvasCommand(SpaceCanvas canvas) {
		this.canvas = canvas;
	}

	public void execute() {
		elements.clear();
		for (Element e : canvas.getSpace().getElements()) {
			elements.add(e);
		}
		canvas.clear();
	}

	public String getLabel() {
		return "Clear Canvas";
	}

	public void undo() {
		for (Element e : elements) {
			canvas.getSpace().add(e);
		}
		canvas.repaint();
	}

	public void redo() {
		execute();
	}

}
