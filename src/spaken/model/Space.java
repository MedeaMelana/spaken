/* Created on Jun 20, 2008. */
package spaken.model;

import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;

import spaken.model.rendered.Rendered;
import spaken.model.rendered.RenderedPoint;
import spaken.util.ClassFilter;
import spaken.util.FilteredIterable;

public class Space {

	private List<Element> elements;

	public Space() {
		elements = new LinkedList<Element>();
	}

	private void demo() {
		FixedPoint p1 = new FixedPoint(100, 100);
		FixedPoint p2 = new FixedPoint(200, 150);
		Line l = new Line(p1, p2);

		add(p1);
		add(p2);
		add(l);
	}

	private void demo2() {
		Point p1 = new FixedPoint(100, 100);
		Point p2 = new FixedPoint(400, 400);
		Point p3 = new FixedPoint(0, 300);
		Point p4 = new FixedPoint(400, 100);

		Line l1 = new Line(p1, p2);
		Line l2 = new Line(p3, p4);

		Point i = Intersections.intersect(l1, l2);

		Circle c = new Circle(i, i, p3);
		Point[] is = Intersections.intersect(c, l1);

		add(p1, p2, p3, p4);
		add(l1, l2);
		add(i);
		add(c);
		add(is);
	}

	public void add(Element... es) {
		for (Element e : es) {
			elements.add(e);
		}
	}

	public Iterable<Element> getElements() {
		return elements;
	}

	public Iterable<FixedPoint> getFixedPoints() {
		return new FilteredIterable<Element,FixedPoint>(getElements(), new ClassFilter<FixedPoint>(FixedPoint.class));
	}
	
	public Iterable<Point> getPoints() {
		return new FilteredIterable<Element,Point>(getElements(), new ClassFilter<Point>(Point.class));
	}
	
	public Point getPointAt(Pos pos, double distance) {
		return getPointAt(pos, distance, getPoints());
	}

	public FixedPoint getFixedPointAt(Pos pos, double distance) {
		return getPointAt(pos, distance, getFixedPoints());
	}

	private <P extends Point> P getPointAt(Pos pos, double distance, Iterable<P> points) {
		double minD = distance * distance;
		P minP = null;
		for (P p : points) {
			try {
				if (p.getPos().distanceSquared(pos) < minD) {
					minP = p;
				}
			} catch (ImaginaryPointException e) {
			}
		}
		return minP;
	}

	public Iterable<Rendered> render() {
		List<Rendered> rs = new LinkedList<Rendered>();
		for (Element e : elements) {
			try {
				rs.add(e.render());
			} catch (ImaginaryPointException e1) {}
		}

		Collections.sort(rs, new Comparator<Rendered>() {

			public int compare(Rendered o1, Rendered o2) {
				if (o1.getClass() == o2.getClass()) {
					return 0;
				} else if (o1 instanceof RenderedPoint) {
					return 1;
				} else if (o2 instanceof RenderedPoint) {
					return -1;
				} else {
					return 0;
				}
			}
		});

		return rs;
	}

}
