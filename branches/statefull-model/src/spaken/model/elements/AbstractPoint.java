package spaken.model.elements;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.geom.Rectangle2D;

import spaken.model.*;
import spaken.ui.swing.DrawingConstants;

public abstract class AbstractPoint extends AbstractElement<Point> implements
		Point {

	public AbstractPoint(Theorem theorem) {
		super(theorem);
	}

	private Color getColor() {
		if (getType() == Type.DERIVED) {
			return DrawingConstants.BACKGROUND;
		} else {
			return DrawingConstants.CONTROLLABLE;
		}
	}

	public void draw(Graphics2D g, double pixelSize) {
		try {
			Pos pos = getPos();

			double s = pixelSize * DrawingConstants.POINT_SIZE / 2;
			Rectangle2D rect = new Rectangle2D.Double(pos.x - s, pos.y - s,
					2 * s, 2 * s);
			g.setColor(getColor());
			g.fill(rect);
			g.setColor(DrawingConstants.FOREGROUND);
			g.draw(rect);
		} catch (ImaginaryPointException e) {
			return;
		}
	}

	public Type getType() {
		return Type.DERIVED;
	}
}