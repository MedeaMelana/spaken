package spaken.model.rendered;

import java.awt.Graphics2D;
import java.awt.geom.Ellipse2D;

import spaken.model.Pos;

public class RenderedCircle implements Rendered {

	private Pos center;
	private double radius;

	public RenderedCircle(Pos center, double radius) {
		this.center = center;
		this.radius = radius;
	}

	public void draw(Graphics2D g, double pixelSize) {
		double diam = 2 * radius;
		g.draw(new Ellipse2D.Double(center.x - radius, center.y - radius, diam,
				diam));
	}

}
