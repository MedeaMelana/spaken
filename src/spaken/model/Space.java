/* Created on Jun 20, 2008. */
package spaken.model;

import java.awt.Graphics2D;
import java.util.*;

import spaken.model.elements.AssumedPoint;
import spaken.model.elements.Point;
import spaken.model.elements.Point.Type;
import spaken.util.ClassFilter;
import spaken.util.FilteredIterable;

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

	public Collection<Element> getElements() {
		return Collections.unmodifiableList(elements);
	}

	public Iterable<AssumedPoint> getFixedPoints() {
		// only fixed AssumedPoints (i.e. for which isActual returns false)
		return new FilteredIterable<Element, AssumedPoint>(getElements(),
				new ClassFilter<AssumedPoint>(AssumedPoint.class));
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
			Pos pp = p.getPos();
			if (pp != null && pp.distanceSquared(pos) < distance) {
				minP = p;
			}
		}
		return minP;
	}

	public void draw(Graphics2D g, double pixelSize) {
		List<Element> rs = new LinkedList<Element>(elements);
		
		// TODO gesorteerde datastructuur ipv steeds sorteren.
		Collections.sort(rs, new Comparator<Element>() {

			public int compare(Element o1, Element o2) {
				if (o1 instanceof Point) {
					if (o2 instanceof Point) {
						Point p1 = (Point) o1;
						Point p2 = (Point) o2;
						if (p1.getType() == Type.DERIVED && p2.getType() == Type.DERIVED) {
							return 0;
						} else {
							return (p1.getType() == Type.DERIVED) ? -1 : 1;
						}
					} else {
						return 1;
					}
				} else if (o2 instanceof Point) {
					return -1;
				} else {
					return 0;
				}
			}
		});
		
		for (Element element : rs) {
			element.draw(g, pixelSize);
		}
	}

	public void clear() {
		elements.clear();
	}

}
