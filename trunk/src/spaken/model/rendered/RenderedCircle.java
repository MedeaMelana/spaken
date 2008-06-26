package spaken.model.rendered;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.geom.Ellipse2D;

import spaken.model.Pos;

public class RenderedCircle implements Rendered {

	private Pos center;
	private double radius;
	private Color color;

	public RenderedCircle(Pos center, double radius) {
		this(center, radius, Color.BLACK);
	}

	public RenderedCircle(Pos center, double radius, Color color) {
		this.center = center;
		this.radius = radius;
		this.color = color;
	}

	public void draw(Graphics2D g, double pixelSize) {
		double diam = 2 * radius;
		g.setColor(color);
		g.draw(new Ellipse2D.Double(center.x - radius, center.y - radius, diam,
				diam));
	}

}
