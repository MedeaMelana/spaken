package spaken.ui.swing.tools;

import java.awt.Graphics2D;
import java.util.*;

import spaken.model.*;
import spaken.model.elements.*;

public class ApplyTheoremTool extends AbstractTool {
	private Theorem theorem;

	private Group partial;

	private List<Point> assumptions;

	private boolean firstClick;

	public ApplyTheoremTool(String name, Theorem theorem) {
		super("Apply Theorem '" + name + "'");
		this.theorem = theorem;
	}

	@Override
	protected void mouseMoved(Pos current, Pos delta) {
		getCanvas().refresh();
	}

	@Override
	protected void strokeStarted(Pos origin) {
		if (assumptions == null) {
			Point p = getCanvas().getPointAt(origin);
			if (p != null) {
				int n = theorem.getAssumptionCount();
				assumptions = new ArrayList<Point>(n);
				feedPoint(getCanvas().getPointAt(origin));
			}
			firstClick = false;
		}
	}

	private void buildPartial() {
		int num = theorem.getAssumptionCount();
		List<Point> points = new ArrayList<Point>(num);
		points.addAll(assumptions);

		Point mouse = getMousePoint();

		for (int n = points.size(); n < num; n++) {
			points.add(mouse);
		}

		try {
			partial = theorem.applyTheorem(points);
		} catch (UnboundPointException e) {
			System.err.println("ApplyTheoremTool.buildPartial() screwed up.");
			resetState();
		}
	}

	protected void feedPoint(Point p) {
		assumptions.add(p);

		if (assumptions.size() >= theorem.getAssumptionCount()) {
			try {
				Group complete = theorem.applyTheorem(assumptions);
				addGroup(complete);
			} catch (UnboundPointException e) {
				System.err.println("ApplyTheoremTool.feedPoint() screwed up.");
			}
			resetState();
		} else {
			buildPartial();
		}
	}

	@Override
	protected void strokeInProgress(Pos origin, Pos current, Pos delta) {
		getCanvas().refresh();
	}

	@Override
	protected void strokeFinished(Pos origin, Pos end) {
		if (assumptions == null)
			return;

		boolean dragged = !origin.equals(end);

		Point p = getCanvas().getPointAt(end);
		if (p != null) {
			if (firstClick || dragged) {
				feedPoint(p);
			}
		}

		firstClick = true;

		getCanvas().refresh();
	}

	@Override
	public void drawState(Graphics2D g, double pixelSize) {
		if (assumptions == null)
			return;

		for (Point p : assumptions) {
			highlightPoint(g, pixelSize, p);
		}

		if (isMouseInside()) {
			try {
				// TODO outline
				partial.render(getSpace().getPointBinding()).draw(g, pixelSize);
			} catch (ImaginaryPointException e) {
			} catch (UnboundPointException e) {
			}
		}
	}

	@Override
	public void resetState() {
		super.resetState();
		assumptions = null;
		partial = null;
	}

}
