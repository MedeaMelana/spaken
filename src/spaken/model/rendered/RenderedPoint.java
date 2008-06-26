package spaken.model.rendered;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.geom.Rectangle2D;

import spaken.model.Pos;
import spaken.ui.swing.DrawingConstants;

public class RenderedPoint implements Rendered {

	private Pos pos;
	private boolean derived;

	public RenderedPoint(Pos pos, boolean derived) {
		this.pos = pos;
		this.derived = derived;
	}

	public void draw(Graphics2D g, double pixelSize) {
		renderPoint(g, pixelSize, pos, derived ? DrawingConstants.BACKGROUND : DrawingConstants.CONTROLLABLE);
	}

	public static void renderPoint(Graphics2D g, double pixelSize, Pos pos,
			Color fill) {
		double s = DrawingConstants.POINT_SIZE / 2;
		Rectangle2D rect = new Rectangle2D.Double(pos.x - s, pos.y - s, 2 * s,
				2 * s);
		g.setColor(fill);
		g.fill(rect);
		g.setColor(DrawingConstants.FOREGROUND);
		g.draw(rect);
	}

}
