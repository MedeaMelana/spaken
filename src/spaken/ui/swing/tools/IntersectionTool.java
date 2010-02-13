package spaken.ui.swing.tools;

import java.awt.Graphics2D;
import java.util.*;

import spaken.model.*;
import spaken.model.elements.*;
import spaken.ui.swing.SpaceCanvas;
import spaken.util.Iterables;

public class IntersectionTool extends AbstractTool {

	private List<Point> intersections;

	public IntersectionTool() {
		super("Mark Intersection");
		intersections = new LinkedList<Point>();
	}

	private Point closestPoint() {
		return getSpace().getPointAt(getMouse(), intersections,
				getCanvas().getPointSelectSize());
	}

	@Override
	protected void mouseMoved(Pos current, Pos delta) {
		getCanvas().refresh();
	}

	@Override
	protected void strokeFinished(Pos origin, Pos end) {
		Point pt = closestPoint();
		if (pt == null) {
			return;
		}

		addElement(pt);
	}

	@Override
	protected void strokeInProgress(Pos origin, Pos current, Pos delta) {
		getCanvas().repaint();
	}

	@Override
	public void drawState(Graphics2D g, double pixelSize) {
		Pos mouse = getMouse();

		if (mouse != null) {
			Point pt = closestPoint();
			if (pt == null)
				return;

			try {
				pt.outline(g, pixelSize);
			} catch (ImaginaryPointException e) {
			}
		}
	}

	@Override
	public void install(SpaceCanvas canvas) {
		super.install(canvas);

		intersections.clear();

		Iterable<Element> elements = canvas.getSpace().getElements();
		List<Element> others = new LinkedList<Element>();
		Iterables.addAll(others, elements);

		// Calculate and store all possible intersections.
		for (Element e : elements) {
			others.remove(e);
			for (Element o : others) {
				Points ps = (Points) Intersections.intersections(getConstruct(), e, o);
				if (ps == null) continue;
				for (int i = 0; i < ps.getPointCount(); i++) {
					intersections.add(ps.getPoint(i));
				}
			}
		}

	}

	@Override
	public void uninstall(SpaceCanvas canvas) {
		super.uninstall(canvas);
		intersections.clear();
	}

}
