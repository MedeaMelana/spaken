package spaken.ui.swing;

import java.awt.Point;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.geom.AffineTransform;

public class SelectTool extends AbstractTool {

	protected SelectTool() {
		super("Select");
	}

	private TranslateListener translateListener = new TranslateListener();

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

	private class TranslateListener implements MouseListener,
			MouseMotionListener {

		Point mouse;

		public void mousePressed(MouseEvent e) {
			mouse = e.getPoint();
		}

		public void mouseDragged(MouseEvent e) {
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
