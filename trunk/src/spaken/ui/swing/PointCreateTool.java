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

	@Override
	public void mouseReleased(MouseEvent e) {
		FixedPoint pt = new FixedPoint(mouse.inverseTransform(canvas
				.getTransform()));
		addElement(pt);
	}

	@Override
	public void mouseMoved(MouseEvent e) {
		canvas.refresh();
		mouse = new Pos(e.getX(), e.getY());
	}

	@Override
	public void mouseDragged(MouseEvent e) {
		mouseMoved(e);
	}

	@Override
	public void drawState(Graphics2D g, double pixelSize) {
		if (mouse != null) {
			new RenderedPoint(mouse.inverseTransform(canvas.getTransform()),
					true, DrawingConstants.OUTLINE).draw(g, pixelSize);
		}
	}

	@Override
	public void uninstall(SpaceCanvas canvas) {
		super.uninstall(canvas);
		mouse = null;
	}

}
