package spaken.ui.swing.tools;

import java.awt.Point;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.geom.AffineTransform;

import spaken.model.Pos;
import spaken.model.elements.FixedPoint;
import spaken.ui.swing.SpaceCanvas;

public class SelectTool extends AbstractTool {

	private TranslateListener translateListener = new TranslateListener();

	private FixedPoint dragging;

	public SelectTool() {
		super("Select");
	}

	@Override
	public void install(SpaceCanvas canvas) {
		super.install(canvas);
		canvas.addMouseListener(translateListener);
		canvas.addMouseMotionListener(translateListener);
	}

	@Override
	public void uninstall(SpaceCanvas canvas) {
		super.uninstall(canvas);
		canvas.removeMouseListener(translateListener);
		canvas.removeMouseMotionListener(translateListener);
	}

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

	private class TranslateListener implements MouseListener,
			MouseMotionListener {

		Point mouse;

		public void mousePressed(MouseEvent e) {
			mouse = e.getPoint();
		}

		public void mouseDragged(MouseEvent e) {
			if (dragging != null) {
				return;
			}

			Point newMouse = e.getPoint();
			if (mouse != null) {
				AffineTransform xf = getCanvas().getTransform();
				int dx = newMouse.x - mouse.x;
				int dy = newMouse.y - mouse.y;
				double sx = xf.getScaleX();
				double sy = xf.getScaleY();
				getCanvas().getTransform().translate(dx / sx, dy / sy);
				getCanvas().refresh();
			}
			mouse = newMouse;
		}

		public void mouseClicked(MouseEvent e) {
		}

		public void mouseEntered(MouseEvent e) {
		}

		public void mouseExited(MouseEvent e) {
		}

		public void mouseReleased(MouseEvent e) {
		}

		public void mouseMoved(MouseEvent e) {
		}

	}

}
