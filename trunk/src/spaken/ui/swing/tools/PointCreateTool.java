package spaken.ui.swing.tools;

import java.awt.Graphics2D;

import spaken.model.Pos;
import spaken.model.rendered.RenderedPoint;
import spaken.ui.swing.DrawingConstants;

public class PointCreateTool extends AbstractTool {

	public PointCreateTool() {
		super("Create Point");
	}

	@Override
	public void drawState(Graphics2D g, double pixelSize) {
		if (isMouseInside()) {
			new RenderedPoint(getMouse(), RenderedPoint.Type.DERIVED,
					DrawingConstants.OUTLINE).draw(g, pixelSize);
		}
	}

	@Override
	protected void strokeFinished(Pos origin, Pos end) {
		addPoint(end);
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
