package spaken.model.rendered;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.geom.Rectangle2D;

import spaken.model.Pos;
import spaken.ui.swing.DrawingConstants;

public class RenderedPoint implements Rendered {

	private Pos pos;

	private boolean derived;

	private Color color;

	public RenderedPoint(Pos pos, boolean derived, Color color) {
		this.pos = pos;
		this.derived = derived;
		this.color = color;
	}

	public RenderedPoint(Pos pos, boolean derived) {
		this(pos, derived, null);
	}
	
	public boolean isDerived() {
		return derived;
	}

	private Color getColor() {
		if (color != null) {
			return color;
		} else if (derived) {
			return DrawingConstants.BACKGROUND;
		} else {
			return DrawingConstants.CONTROLLABLE;
		}
	}

	public void draw(Graphics2D g, double pixelSize) {
		double s = DrawingConstants.POINT_SIZE / 2;
		Rectangle2D rect = new Rectangle2D.Double(pos.x - s, pos.y - s, 2 * s,
				2 * s);
		g.setColor(getColor());
		g.fill(rect);
		g.setColor(DrawingConstants.FOREGROUND);
		g.draw(rect);
	}

}
