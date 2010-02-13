package spaken.model.elements;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.geom.Rectangle2D;

import spaken.model.ImaginaryPointException;
import spaken.model.Pos;
import spaken.ui.swing.DrawingConstants;

public abstract class AbstractPoint extends AbstractElement implements Point {
	protected Color getPointColor() {
		return DrawingConstants.BACKGROUND;
	}

	public void draw(Graphics2D g, double pixelSize, Color color)
			throws ImaginaryPointException {
		double s = pixelSize * DrawingConstants.POINT_SIZE / 2;
		Pos pos = this.getPos();
		Rectangle2D rect = new Rectangle2D.Double(pos.x - s, pos.y - s, 2 * s,
				2 * s);
		g.setColor(getPointColor());
		g.fill(rect);
		g.setColor(color);
		g.draw(rect);
	}
}
