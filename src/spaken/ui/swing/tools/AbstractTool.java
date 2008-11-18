package spaken.ui.swing.tools;

import java.awt.Graphics2D;
import java.awt.event.*;
import java.io.IOException;
import java.util.Collection;

import spaken.model.*;
import spaken.model.commands.AddElementCommand;
import spaken.model.elements.AssumedPoint;
import spaken.model.elements.Point;
import spaken.model.rendered.Rendered;
import spaken.model.rendered.RenderedPoint;
import spaken.storage.ElementReader;
import spaken.storage.ElementWriter;
import spaken.ui.swing.*;

public abstract class AbstractTool implements Tool {

	private SpaceCanvas canvas;

	private String name;

	private StrokeListener strokeListener = new StrokeListener();

	private Point mousePoint = new MousePoint();

	private KeyListener escapeListener = new KeyAdapter() {

		public void keyPressed(KeyEvent e) {
			if (e.getKeyCode() == KeyEvent.VK_ESCAPE) {
				resetState();
				canvas.refresh();
			}
		}

	};

	protected AbstractTool(String name) {
		this.name = name;
	}

	public void install(SpaceCanvas canvas) {
		this.canvas = canvas;
		canvas.addKeyListener(escapeListener);
		canvas.addMouseListener(strokeListener);
		canvas.addMouseMotionListener(strokeListener);
	}

	public void uninstall(SpaceCanvas canvas) {
		this.canvas = null;
		canvas.removeKeyListener(escapeListener);
		canvas.removeMouseListener(strokeListener);
		canvas.removeMouseMotionListener(strokeListener);
		strokeListener.reset();
	}

	/**
	 * @return The <tt>SpaceCanvas</tt> this tool is installed on, or
	 *         <tt>null</tt> if this tool is not installed.
	 */
	protected SpaceCanvas getCanvas() {
		return canvas;
	}

	/**
	 * @return The <tt>Space</tt> of the current <tt>SpaceCanvas</tt>, or
	 *         <tt>null</tt> if this tool is not installed.
	 */
	protected Space getSpace() {
		if (canvas == null) {
			return null;
		} else {
			return canvas.getSpace();
		}
	}

	public String getName() {
		return name;
	}

	protected void execute(Command command) {
		canvas.getHistory().execute(command);
	}

	protected void addElement(Element e) {
		execute(new AddElementCommand(canvas, e));
	}

	/**
	 * @return The current location of the mouse, in drawing coordinates (i.e.
	 *         inverse transformed with the <tt>AffineTransform</tt> on the
	 *         canvas), or <tt>null</tt> if the mouse is not inside the
	 *         canvas.
	 */
	protected Pos getMouse() {
		return strokeListener.getCurrent();
	}

	/**
	 * @return A <tt>Point</tt> always reflecting the current position of the
	 *         mouse.
	 */
	protected Point getMousePoint() {
		return mousePoint;
	}

	protected boolean isMouseInside() {
		return strokeListener.isMouseInside();
	}

	public void drawState(Graphics2D g, double pixelSize) {
	}

	public void resetState() {
	}

	protected void highlightPoint(Graphics2D g, double pixelSize, Point p) {
		if (p == null) {
			return;
		}
		try {
			new RenderedPoint(p.getPos(), RenderedPoint.Type.DERIVED,
					DrawingConstants.HIGHLIGHT).draw(g, pixelSize);
		} catch (ImaginaryPointException e) {
			// Blah.
		}
	}

	protected void mouseMoved(Pos current, Pos delta) {
	}

	protected void strokeStarted(Pos origin) {
	}

	protected void strokeInProgress(Pos origin, Pos current, Pos delta) {
	}

	protected void strokeFinished(Pos origin, Pos end) {
	}

	private class StrokeListener implements MouseListener, MouseMotionListener {

		private Pos origin;

		private Pos current;

		private boolean mouseInside;

		private Pos setAndDelta(Pos newCurrent) {
			Pos delta = current != null ? newCurrent.subtract(current) : null;
			current = newCurrent;
			return delta;
		}

		public void reset() {
			origin = null;
			current = null;
		}

		public void mouseMoved(MouseEvent e) {
			Pos delta = setAndDelta(canvas.mouse2space(new Pos(e.getPoint())));
			AbstractTool.this.mouseMoved(current, delta);
		}

		public void mousePressed(MouseEvent e) {
			origin = canvas.mouse2space(new Pos(e.getPoint()));
			current = origin;
			strokeStarted(origin);
		}

		public void mouseReleased(MouseEvent e) {
			strokeFinished(origin, current);
			origin = null;
		}

		public void mouseDragged(MouseEvent e) {
			Pos delta = setAndDelta(canvas.mouse2space(new Pos(e.getPoint())));
			strokeInProgress(origin, current, delta);
		}

		/** During a stroke, returns the position where the mouse was pressed. */
		public Pos getOrigin() {
			return origin;
		}

		/** Returns the mouse's current position. */
		public Pos getCurrent() {
			return current;
		}

		public boolean isMouseInside() {
			return mouseInside;
		}

		public void mouseClicked(MouseEvent e) {
		}

		public void mouseEntered(MouseEvent e) {
			mouseInside = true;
			getCanvas().refresh();
		}

		public void mouseExited(MouseEvent e) {
			mouseInside = false;
			getCanvas().refresh();
		}

	}

	private class MousePoint implements Point {

		public Pos getPos() throws ImaginaryPointException {
			if (isMouseInside()) {
				return getMouse();
			} else {
				throw new ImaginaryPointException(this);
			}
		}

		public void readElement(ElementReader in) throws IOException {
			throw new UnsupportedOperationException();
		}

		public Rendered render() throws ImaginaryPointException {
			throw new UnsupportedOperationException();
		}

		public void writeElement(ElementWriter out) throws IOException {
			throw new UnsupportedOperationException();
		}

	}

}
