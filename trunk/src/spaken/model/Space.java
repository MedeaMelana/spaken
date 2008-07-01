/* Created on Jun 20, 2008. */
package spaken.model;

import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;

import spaken.model.intersections.Intersections;
import spaken.model.rendered.Rendered;
import spaken.model.rendered.RenderedPoint;
import spaken.ui.swing.DrawingConstants;
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

	private void demoMiddenloodlijn() {
		Point p1 = new FixedPoint(100, 100);
		Point p2 = new FixedPoint(400, 300);

		Line l1 = new Line(p1, p2);

		Circle c1 = new Circle(p1, p1, p2);
		Circle c2 = new Circle(p2, p2, p1);

		Point[] is = Intersections.intersect(c1, c2);

		Line mll = new Line(is[0], is[1]);

		add(p1, p2);
		add(l1);
		add(c1, c2);
		add(is);
		add(mll);
	}

	private void demoParallel() {
		Point p1 = new FixedPoint(100, 200);
		Point p2 = new FixedPoint(400, 300);
		Point p3 = new FixedPoint(200, 100);

		Circle c1 = new Circle(p3, p1, p2);
		Circle c2 = new Circle(p2, p1, p3);
		Point p4 = Intersections.intersect(c1, c2)[0];

		add(p1, p2, p3, p4, c1, c2, new Line(p1, p2), new Line(p3, p4));
	}

	public void add(Element... es) {
		for (Element e : es) {
			elements.add(e);
		}
	}

	public void remove(Element... es) {
		for (Element e : es) {
			elements.remove(e);
		}
	}

	public Iterable<Element> getElements() {
		return elements;
	}

	public Iterable<FixedPoint> getFixedPoints() {
		return new FilteredIterable<Element, FixedPoint>(getElements(),
				new ClassFilter<FixedPoint>(FixedPoint.class));
	}

	public Iterable<Point> getPoints() {
		return new FilteredIterable<Element, Point>(getElements(),
				new ClassFilter<Point>(Point.class));
	}

	public Point getPointAt(Pos pos) {
		return getPointAt(pos, getPoints());
	}

	public FixedPoint getFixedPointAt(Pos pos) {
		return getPointAt(pos, getFixedPoints());
	}

	public <P extends Point> P getPointAt(Pos pos, Iterable<P> points) {
		double limit = DrawingConstants.POINT_SELECT_SIZE;
		limit = limit * limit;
		P minP = null;
		for (P p : points) {
			try {
				Pos pp = p.getPos();
				if (pp.distanceSquared(pos) < limit) {
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
			} catch (ImaginaryPointException e1) {
			}
		}

		Collections.sort(rs, new Comparator<Rendered>() {

			public int compare(Rendered o1, Rendered o2) {
				if (o1 instanceof RenderedPoint) {
					if (o2 instanceof RenderedPoint) {
						RenderedPoint p1 = (RenderedPoint) o1;
						RenderedPoint p2 = (RenderedPoint) o2;
						if (p1.isDerived() == p2.isDerived()) {
							return 0;
						} else {
							return p1.isDerived() ? -1 : 1;
						}
					} else {
						return 1;
					}
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
