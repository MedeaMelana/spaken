package spaken.ui.swing;

import java.awt.event.MouseEvent;

import spaken.model.FixedPoint;
import spaken.model.Pos;

public class PointMoveTool extends AbstractTool {

	protected PointMoveTool() {
		super("Move Point");
	}

	private FixedPoint dragging;

	private Pos mouse;

	public void mousePressed(MouseEvent e) {
		mouse = new Pos(e.getX(), e.getY());
		dragging = canvas.getSpace().getFixedPointAt(mouse,
				DrawingConstants.POINT_SELECT_SIZE);
	}

	@Override
	public void mouseDragged(MouseEvent e) {
		if (dragging != null) {
			Pos mouseNew = new Pos(e.getX(), e.getY());
			Pos diff = mouseNew.subtract(mouse);
			dragging.setPos(dragging.getPos().add(diff));
			canvas.refresh();
			mouse = mouseNew;
		}
	}

	@Override
	public void mouseReleased(MouseEvent e) {
		dragging = null;
		mouse = null;
	}

}