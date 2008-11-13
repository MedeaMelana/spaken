/* Created on Jun 20, 2008. */
package spaken.model.elements;

import java.io.IOException;
import java.util.List;

import spaken.model.*;
import spaken.model.rendered.RenderedPoint;
import spaken.model.rendered.RenderedPoint.Type;
import spaken.storage.ElementReader;
import spaken.storage.ElementWriter;

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
	
	public Point makePluggableCopy(List<PluggablePoint> collect) {
		PluggablePoint p = new PluggablePoint(this);
		collect.add(p);
		return p;
	}
	
	public void writeElement(ElementWriter out) throws IOException {
		out.writePos(pos);
	}
	
	public void readElement(ElementReader in) throws IOException {
		pos = in.readPos();
	}
}
