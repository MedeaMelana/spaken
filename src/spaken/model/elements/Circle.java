package spaken.model.elements;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.geom.Ellipse2D;

import spaken.model.*;

public class Circle extends AbstractElement {

	private final Point center, distFrom, distTo;

	Circle(Point center, Point distFrom, Point distTo) {
		this.center = center;
		this.distFrom = distFrom;
		this.distTo = distTo;
	}

	public Point getCenter() {
		return center;
	}

	public Point getDistFrom() {
		return distFrom;
	}

	public Point getDistTo() {
		return distTo;
	}

	public <Elem, Err extends Throwable> Elem visit(Spaken<Elem, Err> sp)
			throws Err {
		Elem c = center.visit(sp);
		Elem df = distFrom.visit(sp);
		Elem dt = distTo.visit(sp);
		return sp.circle(c, df, dt);
	}

	public void draw(Graphics2D g, double pixelSize, Color color)
			throws ImaginaryPointException {
		Pos c = center.getPos();
		Pos from = distFrom.getPos();
		Pos to = distTo.getPos();
		double radius = from.distance(to);
		double diam = 2 * radius;
		g.setColor(color);
		g.draw(new Ellipse2D.Double(c.x - radius, c.y - radius, diam, diam));
	}

}
