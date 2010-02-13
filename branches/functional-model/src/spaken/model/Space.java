package spaken.model;

import java.awt.Graphics2D;
import java.util.*;

import spaken.model.elements.*;
import spaken.util.*;

public class Space {
	private List<AssumedPoint> inputs;

	private List<Point> otherPoints;

	private List<Element> otherElements;

	public Space() {
		otherElements = new LinkedList<Element>();
		inputs = new LinkedList<AssumedPoint>();
		otherPoints = new LinkedList<Point>();
	}

	public void add(Element... es) {
		for (Element e : es) {
			if (e instanceof AssumedPoint) {
				inputs.add((AssumedPoint) e);
			} else if (e instanceof Point) {
				otherPoints.add((Point) e);
			} else {
				otherElements.add(e);
			}
		}
	}

	public void remove(Element... es) {
		for (Element e : es) {
			inputs.remove(e);
			otherPoints.remove(e);
			otherElements.remove(e);
		}
	}

	public Iterable<Element> getElements() {
		return Iterables.catIterables(Collections
				.unmodifiableList(otherElements), getPoints());
	}

	public Iterable<Point> getPoints() {
		return Iterables.catIterables(
				Collections.unmodifiableList(otherPoints), getInputPoints());
	}

	public Iterable<AssumedPoint> getInputPoints() {
		return Collections.unmodifiableList(inputs);
	}

	public Point getPointAt(Pos pos, double distance) {
		return getPointAt(pos, getPoints(), distance);
	}

	public AssumedPoint getFixedPointAt(Pos pos, double distance) {
		return getPointAt(pos, getInputPoints(), distance);
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

	public void draw(Graphics2D g, double pixelSize) {
		for (Element e : getElements()) {
			try {
				e.draw(g, pixelSize, false);
			} catch (ImaginaryPointException ex) {
				// No problem
			}
		}
	}

	public void clear() {
		otherElements.clear();
	}

}
