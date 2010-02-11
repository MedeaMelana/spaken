package spaken.model.rendered;

import java.awt.*;
import java.awt.geom.Rectangle2D;

import spaken.model.Pos;
import spaken.ui.swing.DrawingConstants;

public class RenderedPoint implements Rendered {

	private Pos pos;
	
	public enum Type {
		FIXED(false), DERIVED(true), PLUGGABLE(true);
		
		private boolean derived; 
		
		Type(boolean derived) {
			this.derived = derived;
		}
	}
	
	private Type type;

	private Color color;

	public RenderedPoint(Pos pos, Type type, Color color) {
		this.pos = pos;
		this.type = type;
		this.color = color;
	}

	public RenderedPoint(Pos pos, Type type) {
		this(pos, type, null);
	}
	
	public boolean isDerived() {
		return type.derived;
	}

	private Color getColor() {
		if (color != null) {
			return color;
		} else if (isDerived()) {
			return DrawingConstants.BACKGROUND;
		} else {
			return DrawingConstants.CONTROLLABLE;
		}
	}

	public void draw(Graphics2D g, double pixelSize) {
		double s = pixelSize * DrawingConstants.POINT_SIZE / 2;
		Rectangle2D rect = new Rectangle2D.Double(pos.x - s, pos.y - s, 2 * s,
				2 * s);
		g.setColor(getColor());
		g.fill(rect);
		g.setColor(DrawingConstants.FOREGROUND);
		g.draw(rect);
	}

}
