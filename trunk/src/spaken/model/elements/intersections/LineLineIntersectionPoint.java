package spaken.model.elements.intersections;

import java.util.List;

import spaken.model.*;
import spaken.model.elements.*;

public class LineLineIntersectionPoint extends AbstractPoint {
	
	private Line l1, l2;
	
	LineLineIntersectionPoint(Line l1, Line l2) {
		this.l1 = l1;
		this.l2 = l2;
	}

	public Pos getPos() throws ImaginaryPointException {
		Pos p1 = l1.getP1().getPos();
		Pos p2 = l1.getP2().getPos();
		Pos p3 = l2.getP1().getPos();
		Pos p4 = l2.getP2().getPos();

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
	
	public Element[] getDependencies() {
		return new Element[] {l1, l2};
	}
	
	public Point makePluggableCopy(List<PluggablePoint> collect) {
		Line l1c = l1.makePluggableCopy(collect);
		Line l2c = l2.makePluggableCopy(collect);
		
		return new LineLineIntersectionPoint(l1c, l2c);
	}
}
