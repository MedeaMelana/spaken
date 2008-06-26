package spaken.model.rendered;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.geom.Rectangle2D;

import spaken.model.Pos;

public class RenderedPoint implements Rendered {

	private static final Color FIXED_COLOR = new Color(0x7f7fff);

	private Pos pos;
	private boolean derived;

	public RenderedPoint(Pos pos, boolean derived) {
		this.pos = pos;
		this.derived = derived;
	}

	public void draw(Graphics2D g, double pixelSize) {
		renderPoint(g, pixelSize, pos, derived ? Color.WHITE : FIXED_COLOR);
	}

	public static void renderPoint(Graphics2D g, double pixelSize, Pos pos,
			Color fill) {
		double s = pixelSize * 2.5;
		Rectangle2D rect = new Rectangle2D.Double(pos.x - s, pos.y - s, 2 * s,
				2 * s);
		g.setColor(fill);
		g.fill(rect);
		g.setColor(Color.BLACK);
		g.draw(rect);
	}

}
