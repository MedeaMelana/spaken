/* Created on Jun 20, 2008. */
package spaken.model;

import java.util.List;

import spaken.model.rendered.Rendered;
import spaken.model.rendered.RenderedPoint;
import spaken.model.rendered.RenderedPoint.Type;

public class FixedPoint extends AbstractPoint {

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
	
	@Override
	protected Type getRenderedPointType() {
		return RenderedPoint.Type.FIXED;
	}
	
	public Point[] getDependencies() {
		return new Point[] {};
	}
}
