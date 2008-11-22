package spaken.model;

public class UnboundPointException extends Exception {
	private static final String MESSAGE = "Unbound point";

	private final PointBinding binding;

	private final int index;

	public UnboundPointException(PointBinding binding, int i) {
		super(MESSAGE + " " + i);
		this.binding = binding;
		this.index = i;
	}

	public UnboundPointException(int i) {
		this(null, i);
	}

	public UnboundPointException() {
		super(MESSAGE);
		this.binding = null;
		this.index = -1;
	}

	public PointBinding getBinding() {
		return binding;
	}

	public int getIndex() {
		return index;
	}

}
