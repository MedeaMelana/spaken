package spaken.ui.swing.tools;

import java.awt.Graphics2D;

import spaken.model.*;
import spaken.model.elements.*;

// Idea:
// One stroke creates a circle from two points.
// Three stokes create a circle from three points.
public class CreateCircleTool extends AbstractTool {

	private Point distFrom, distTo;

	public CreateCircleTool() {
		super("Create Circle");
	}

	@Override
	protected void mouseMoved(Pos current, Pos delta) {
		getCanvas().refresh();
	}

	@Override
	protected void strokeStarted(Pos origin) {
		if (distFrom == null) {
			distFrom = getCanvas().getPointAt(origin);
		}
	}

	@Override
	protected void strokeInProgress(Pos origin, Pos current, Pos delta) {
		getCanvas().refresh();
	}

	@Override
	protected void strokeFinished(Pos origin, Pos end) {
		Point p = getCanvas().getPointAt(end);

		if (p == null) {
			return;
		} else if (distTo != null) {
			Element c = getConstruct().circle(p, distFrom, distTo);
			addElement(c);
			resetState();
		} else if (distFrom != null && distFrom != p) {
			distTo = p;
		} else if (distFrom == null) {
			distFrom = p;
		}
		getCanvas().refresh();
	}

	@Override
	public void drawState(Graphics2D g, double pixelSize) {
		highlightPoint(g, pixelSize, distFrom);
		highlightPoint(g, pixelSize, distTo);

		if (isMouseInside()) {
			try {
				Element mouse = getMousePoint();

				if (distTo != null) {
					getConstruct().circle(mouse, distFrom, distTo).outline(g, pixelSize);
				} else if (distFrom != null) {
					getConstruct().circle(mouse, distFrom, mouse).outline(g, pixelSize);
				}
			} catch (ImaginaryPointException e) {
			}
		}
	}

	@Override
	public void resetState() {
		super.resetState();
		distFrom = null;
		distTo = null;
	}

}
