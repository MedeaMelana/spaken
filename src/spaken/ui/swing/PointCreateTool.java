package spaken.ui.swing;

import java.awt.Graphics2D;

import spaken.model.FixedPoint;
import spaken.model.Pos;
import spaken.model.rendered.RenderedPoint;

public class PointCreateTool extends AbstractTool {

	protected PointCreateTool() {
		super("Create Point");
	}

	@Override
	public void drawState(Graphics2D g, double pixelSize) {
		if (isMouseInside()) {
			new RenderedPoint(getMouse(), true, DrawingConstants.OUTLINE).draw(
					g, pixelSize);
		}
	}

	@Override
	protected void strokeFinished(Pos origin, Pos end) {
		addElement(new FixedPoint(end));
	}

	@Override
	protected void strokeInProgress(Pos origin, Pos current, Pos delta) {
		getCanvas().refresh();
	}

	@Override
	protected void mouseMoved(Pos current, Pos delta) {
		getCanvas().refresh();
	}

}
