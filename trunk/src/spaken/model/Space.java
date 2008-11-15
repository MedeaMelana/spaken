/* Created on Jun 20, 2008. */
package spaken.model;

import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;

import spaken.model.elements.AssumedPoint;
import spaken.model.elements.Point;
import spaken.model.rendered.Rendered;
import spaken.model.rendered.RenderedPoint;
import spaken.util.*;

public class Space {

	private List<Element> elements;

	public Space() {
		elements = new LinkedList<Element>();
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

	public Iterable<AssumedPoint> getFixedPoints() {
		// only fixed AssumedPoints (i.e. for which isActual returns false)
		return new FilteredIterable<Element, AssumedPoint>(getElements(),
				new Filter<Element, AssumedPoint>() {

					public boolean accepts(Element a) {
						if (a instanceof AssumedPoint) {
							return !((AssumedPoint) a).isActual();
						}
						return false;
					}

					public AssumedPoint map(Element a) {
						return (AssumedPoint) a;
					}

				});
	}

	public Iterable<Point> getPoints() {
		return new FilteredIterable<Element, Point>(getElements(),
				new ClassFilter<Point>(Point.class));
	}

	public Point getPointAt(Pos pos, double distance) {
		return getPointAt(pos, getPoints(), distance);
	}

	public AssumedPoint getFixedPointAt(Pos pos, double distance) {
		return getPointAt(pos, getFixedPoints(), distance);
	}

	public <P extends Point> P getPointAt(Pos pos, Iterable<P> points,
			double distance) {
		distance = distance * distance;
		P minP = null;
		for (P p : points) {
			try {
				Pos pp = p.getPos();
				if (pp.distanceSquared(pos) < distance) {
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

	public void clear() {
		elements.clear();
	}

}
