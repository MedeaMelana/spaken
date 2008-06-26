package spaken.model.rendered;

import java.awt.Graphics2D;
import java.awt.geom.Line2D;

import spaken.model.Pos;

public class RenderedLine implements Rendered {

	private Pos p1;
	private Pos p2;

	public RenderedLine(Pos p1, Pos p2) {
		this.p1 = p1;
		this.p2 = p2;
	}

	public void draw(Graphics2D g, double pixelSize) {
		g.draw(new Line2D.Double(p1.x, p1.y, p2.x, p2.y));
	}

}
