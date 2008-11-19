/* Created on Jun 20, 2008. */
package spaken.model;

import java.util.*;

import spaken.model.elements.AssumedPoint;
import spaken.model.elements.Point;
import spaken.model.rendered.Rendered;
import spaken.model.rendered.RenderedPoint;
import spaken.util.*;

public class Space {

	private List<Element> elements;

	private PointBinding<Pos> pointBinding;

	private List<Pos> pointPositions;

	public Space() {
		elements = new LinkedList<Element>();
		pointPositions = new ArrayList<Pos>();
		pointBinding = new ListPointBinding<Pos>(pointPositions);
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
			try {
				Pos pp = p.getPos(pointBinding);
				if (pp.distanceSquared(pos) < distance) {
					minP = p;
				}
			} catch (ImaginaryPointException e) {
			} catch (UnboundPointException e) {
			}
		}
		return minP;
	}

	public Iterable<Rendered> render() {
		List<Rendered> rs = new LinkedList<Rendered>();
		for (Element<?> e : elements) {
			try {
				rs.add(e.render(pointBinding));
			} catch (ImaginaryPointException e1) {
			} catch (UnboundPointException e1) {
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

	public PointBinding<Pos> getPointBinding() {
		return pointBinding;
	}

	/* public Pos getPos(Point p) throws ImaginaryPointException, UnboundPointException {
		return p.getPos(pointBinding);
	} */

}
