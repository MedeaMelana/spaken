package spaken.ui.swing;

import java.awt.Color;
import static java.awt.Color.*;

public class DrawingConstants {
	/** Canvas background color. */
	public static final Color BACKGROUND = WHITE;

	/** Color of 'normal' elements. */
	public static final Color FOREGROUND = BLACK;

	/** Color of elements that are being created/dragged. */
	public static final Color OUTLINE = new Color(0xb0b0b0);

	/** Color of highlight around 'touched' elements. */
	public static final Color HIGHLIGHT = new Color(0xff4040);

	/** Color of elements that can be dragged. */
	public static final Color CONTROLLABLE = new Color(0x4040ff);

	public static final Color AXIS = OUTLINE;

	public static final double POINT_SIZE = 5;

	public static final double POINT_SELECT_SIZE = 10;

	public static final double ZOOM_MAX = 50;

	public static final double ZOOM_MIN = 1e-12;

	public static final double ZOOM_INITIAL = 1; // 1e-6;
}
