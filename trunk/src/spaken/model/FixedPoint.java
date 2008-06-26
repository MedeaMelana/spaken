/* Created on Jun 20, 2008. */
package spaken.model;

import spaken.model.rendered.Rendered;
import spaken.model.rendered.RenderedPoint;

public class FixedPoint implements Point {

	private Pos pos;

	public FixedPoint(double x, double y) {
		this(new Pos(x, y));
	}

	public FixedPoint(Pos pos) {
		this.pos = pos;
	}

	public Pos getPos() {
		return pos;
	}

	public void setPos(Pos pos) {
		this.pos = pos;
	}

	public double getX() {
		return pos.x;
	}

	public void setX(double x) {
		pos = pos.setX(x);
	}

	public double getY() {
		return pos.y;
	}

	public void setY(double y) {
		pos = pos.setY(y);
	}

	public Rendered render() throws ImaginaryPointException {
		return new RenderedPoint(getPos(), false);
	}

}
