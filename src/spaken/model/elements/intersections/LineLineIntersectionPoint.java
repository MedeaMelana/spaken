package spaken.model.elements.intersections;

import java.io.IOException;
import java.util.Set;

import spaken.model.*;
import spaken.model.elements.*;
import spaken.storage.ElementReader;
import spaken.storage.ElementWriter;

public class LineLineIntersectionPoint extends AbstractPoint {

	private Line l1, l2;

	/**
	 * Only used internally for reading and writing!
	 */
	public LineLineIntersectionPoint() {
	}

	LineLineIntersectionPoint(Line l1, Line l2) {
		this.l1 = l1;
		this.l2 = l2;
	}

	public Pos getPos(PointBinding binding) throws ImaginaryPointException,
			UnboundPointException {
		Pos p1 = l1.getP1().getPos(binding);
		Pos p2 = l1.getP2().getPos(binding);
		Pos p3 = l2.getP1().getPos(binding);
		Pos p4 = l2.getP2().getPos(binding);

		// formule van
		// http://en.wikipedia.org/w/index.php?title=Line-line_intersection&oldid=210305729

		double w = (p1.x - p2.x) * (p3.y - p4.y) - (p1.y - p2.y)
				* (p3.x - p4.x);

		if (w == 0) {
			throw new ImaginaryPointException();
		}

		double u = p1.x * p2.y - p1.y * p2.x;
		double v = p3.x * p4.y - p3.y * p4.x;

		double ax = p3.x - p4.x;
		double bx = p1.x - p2.x;
		double ay = p3.y - p4.y;
		double by = p1.y - p2.y;

		return new Pos((u * ax - bx * v) / w, (u * ay - by * v) / w);
	}

	public void collectAssumptions(Set<AssumedPoint> collect) {
		l1.collectAssumptions(collect);
		l2.collectAssumptions(collect);
	}

	public Point instantiate(PointBinding binding) throws UnboundPointException {
		return new LineLineIntersectionPoint(l1.instantiate(binding), l2
				.instantiate(binding));
	}

	public void writeElement(ElementWriter out) throws IOException {
		out.writeRef(l1);
		out.writeRef(l2);
	}

	public void readElement(ElementReader in) throws IOException {
		l1 = (Line) in.readRef();
		l2 = (Line) in.readRef();
	}

}
