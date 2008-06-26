package spaken.model.rendered;

import java.awt.Graphics2D;
import java.awt.geom.Rectangle2D;

import spaken.model.Pos;

public class RenderedPoint implements Rendered {

	private Pos pos;

	public RenderedPoint(Pos pos) {
		this.pos = pos;
	}

	public void draw(Graphics2D g, double pixelSize) {
		double s = pixelSize * 3;
		g.fill(new Rectangle2D.Double(pos.x - pixelSize * 1.5, pos.y
				- pixelSize * 1.5, s, s));
	}

}
