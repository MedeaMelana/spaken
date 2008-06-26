package spaken.ui.swing;

import java.awt.Graphics2D;
import java.awt.event.MouseEvent;

import spaken.model.*;
import spaken.model.rendered.RenderedPoint;

public class PointCreateTool extends AbstractTool {

	private Pos mouse;

	protected PointCreateTool() {
		super("Create Point");
	}

	public void mouseClicked(MouseEvent e) {
		FixedPoint pt = new FixedPoint(e.getX(), e.getY());
		canvas.getSpace().add(pt);
		canvas.refresh();
	}

	@Override
	public void mouseMoved(MouseEvent e) {
		canvas.refresh();
		mouse = new Pos(e.getX(), e.getY());
	}

	@Override
	public void drawState(Graphics2D g, double pixelSize) {
		if (mouse != null) {
			new RenderedPoint(mouse, true, DrawingConstants.OUTLINE).draw(g, pixelSize);
		}
	}

	@Override
	public void uninstall(SpaceCanvas canvas) {
		super.uninstall(canvas);
		mouse = null;
	}

}
