package spaken.ui.swing;

import java.awt.Graphics2D;

import spaken.model.ImaginaryPointException;
import spaken.model.Line;
import spaken.model.Point;
import spaken.model.Pos;
import spaken.model.rendered.RenderedLine;

public class CreateLineTool extends AbstractTool {

	Point p1;

	protected CreateLineTool() {
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
				addElement(new Line(p1, p2));
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
				Pos mouse = getMouse();
				try {
					new RenderedLine(p1.getPos(), mouse,
							DrawingConstants.OUTLINE).draw(g, pixelSize);
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
