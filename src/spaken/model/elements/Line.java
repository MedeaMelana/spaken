package spaken.model.elements;

import java.awt.Graphics2D;
import java.awt.geom.Line2D;

import spaken.model.*;
import spaken.ui.swing.DrawingConstants;

public class Line extends AbstractElement {
	private static final double VERY_LARGE_NUMBER = Math.pow(10, 4);

	private final Point p1, p2;

	Line(Point p1, Point p2) {
		if (p1 == p2) {
			throw new IllegalArgumentException(
					"Don't create line from two equal points.");
		}
		this.p1 = p1;
		this.p2 = p2;
	}

	public Point getP1() {
		return p1;
	}

	public Point getP2() {
		return p2;
	}

	public <Elem, Err extends Throwable> Elem visit(Spaken<Elem, Err> sp)
			throws Err {
		Elem r1 = p1.visit(sp);
		Elem r2 = p2.visit(sp);
		return sp.line(r1, r2);
	}

	public void draw(Graphics2D g, double pixelSize, boolean highlight)
			throws ImaginaryPointException {
		Pos v1 = p1.getPos();
		Pos v2 = p2.getPos();
		Pos d = v2.subtract(v1);
		try {
			d = d.normalise().scale(VERY_LARGE_NUMBER);
		} catch (NullVectorException e) {
			d = Pos.ZERO;
		}
		Pos p1ext = v1.add(d);
		Pos p2ext = v2.subtract(d);

		g.setColor(DrawingConstants.FOREGROUND);
		g.draw(new Line2D.Double(p1ext.x, p1ext.y, p2ext.x, p2ext.y));
	}
}
