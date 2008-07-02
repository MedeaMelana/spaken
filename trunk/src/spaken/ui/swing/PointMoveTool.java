package spaken.ui.swing;

import spaken.model.FixedPoint;
import spaken.model.Pos;

public class PointMoveTool extends AbstractTool {

	protected PointMoveTool() {
		super("Move Point");
	}

	private FixedPoint dragging;

	@Override
	protected void strokeStarted(Pos origin) {
		dragging = getCanvas().getFixedPointAt(origin);
	}

	@Override
	protected void strokeInProgress(Pos origin, Pos current, Pos delta) {
		if (dragging != null) {
			dragging.setPos(dragging.getPos().add(delta));
			getCanvas().refresh();
		}
	}

	@Override
	protected void strokeFinished(Pos origin, Pos end) {
		dragging = null;
	}

}
