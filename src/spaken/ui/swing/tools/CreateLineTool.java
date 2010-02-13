package spaken.ui.swing.tools;

import java.awt.Graphics2D;

import spaken.model.*;
import spaken.model.elements.*;

public class CreateLineTool extends AbstractTool {

	Point p1;

	public CreateLineTool() {
		super("Create Line");
	}

	@Override
	protected void strokeStarted(Pos origin) {
		if (p1 == null) {
			p1 = getCanvas().getPointAt(origin);
		}
	}

	@Override
	protected void strokeInProgress(Pos origin, Pos current, Pos delta) {
		getCanvas().refresh();
	}

	@Override
	protected void strokeFinished(Pos origin, Pos end) {
		if (p1 != null) {
			Point p2 = getCanvas().getPointAt(end);
			if (p2 != null && p2 != p1) {
				Element e = getConstruct().line(p1, p2);
				addElement(e);
				p1 = null;
			}
		} else {
			p1 = getCanvas().getPointAt(end);
		}
	}

	@Override
	protected void mouseMoved(Pos current, Pos delta) {
		getCanvas().refresh();
	}

	@Override
	public void drawState(Graphics2D g, double pixelSize) {
		if (p1 != null) {
			highlightPoint(g, pixelSize, p1);

			if (isMouseInside()) {
				Element mouse = getMousePoint();
				try {
					getConstruct().line(p1, mouse).outline(g, pixelSize);
				} catch (ImaginaryPointException e) {
				}
			}
		}
	}

	@Override
	public void resetState() {
		p1 = null;
	}

}
