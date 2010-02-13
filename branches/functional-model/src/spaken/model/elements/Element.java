package spaken.model.elements;

import java.awt.Graphics2D;

import spaken.model.ImaginaryPointException;
import spaken.model.Spaken;
import spaken.util.Unique;

public interface Element {
	public <Elem, Err extends Throwable> Elem visit(Spaken<Elem, Err> sp)
			throws Err;

	public Unique getId();

	public void draw(Graphics2D g, double pixelSize)
			throws ImaginaryPointException;

	public void outline(Graphics2D g, double pixelSize)
			throws ImaginaryPointException;

	public void highlight(Graphics2D g, double pixelSize)
			throws ImaginaryPointException;
}
