package spaken.model.elements;

import java.awt.Color;
import java.awt.Graphics2D;

import spaken.model.ImaginaryPointException;
import spaken.ui.swing.DrawingConstants;
import spaken.util.Unique;

public abstract class AbstractElement implements Element {
	private final Unique id;

	public AbstractElement() {
		id = Unique.create();
	}

	public Unique getId() {
		return id;
	}

	public boolean equals(Object other) {
		/*
		 * Compares references. For Elements other than AssumedPoints, this may
		 * be overridden by a smarter check that compares the elements on which
		 * it depends.
		 */
		if (other != null && other instanceof Element) {
			return id.equals(((Element) other).getId());
		}
		return false;
	}
	
	public void draw(Graphics2D g, double pixelSize) throws ImaginaryPointException {
		draw(g, pixelSize, DrawingConstants.FOREGROUND);
	}
	
	public void highlight(Graphics2D g, double pixelSize) throws ImaginaryPointException {
		draw(g, pixelSize, DrawingConstants.HIGHLIGHT);
	}
	
	public void outline(Graphics2D g, double pixelSize) throws ImaginaryPointException {
		draw(g, pixelSize, DrawingConstants.OUTLINE);
	}
	
	protected abstract void draw(Graphics2D g, double pixelSize, Color color) throws ImaginaryPointException;
}
