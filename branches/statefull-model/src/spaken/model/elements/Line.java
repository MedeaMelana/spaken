package spaken.model.elements;

import java.awt.Graphics2D;
import java.awt.geom.Line2D;
import java.util.Set;

import spaken.model.*;
import spaken.ui.swing.DrawingConstants;

public class Line extends AbstractElement<Line> implements
		Dependency<Point> {

	private Point p1;

	private Point p2;

	/**
	 * Only used internally for reading and writing!
	 */
	public Line() {
	}

	public Line(Point p1, Point p2) {
		if (p1 == p2) {
			throw new IllegalArgumentException(
					"Don't create line from two equal points.");
		}
		this.p1 = p1;
		this.p2 = p2;

		p1.addDependency(this);
		p2.addDependency(this);
	}

	public Point getP1() {
		return p1;
	}

	public Point getP2() {
		return p2;
	}

	// public Rendered render(PointBinding<Pos> binding)
	// throws ImaginaryPointException, UnboundPointException {
	// return new RenderedLine(p1.getPos(binding), p2.getPos(binding));
	// }

	public void collectAssumptions(Set<AssumedPoint> collect) {
		p1.collectAssumptions(collect);
		p2.collectAssumptions(collect);
	}

	// public Line instantiate(PointBinding<Point> binding)
	// throws UnboundPointException {
	// return new Line(p1.instantiate(binding), p2.instantiate(binding));
	// }
	//
	// public void writeElement(ElementWriter out) throws IOException {
	// out.writeRef(p1);
	// out.writeRef(p2);
	// }
	//
	// public void readElement(ElementReader in) throws IOException {
	// p1 = (Point) in.readRef();
	// p2 = (Point) in.readRef();
	// }

	public void elementChanged(Point e) {
		notifyDependencies(this);
	}

	private static final double VERY_LARGE_NUMBER = Math.pow(10,4);

	public void draw(Graphics2D g, double pixelSize) {
		try {
			Pos pos1 = p1.getPos();
			Pos pos2 = p2.getPos();
			
			Pos d = pos2.subtract(pos1);
			try {
				d = d.normalise().scale(VERY_LARGE_NUMBER);
			} catch (NullVectorException e) {
				d = Pos.ZERO;
			}
			Pos p1ext = pos1.add(d);
			Pos p2ext = pos2.subtract(d);
	
			g.setColor(DrawingConstants.FOREGROUND);
			g.draw(new Line2D.Double(p1ext.x, p1ext.y, p2ext.x, p2ext.y));
		} catch (ImaginaryPointException e) {
			return;
		}
	}

}
