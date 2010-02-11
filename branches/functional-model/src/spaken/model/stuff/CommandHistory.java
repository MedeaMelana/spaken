package spaken.model;

import java.util.LinkedList;
import java.util.List;
import java.util.Stack;

/** Provides undo and redo support. */
public class CommandHistory {

	private Stack<Command> undoes = new Stack<Command>();

	private Stack<Command> redoes = new Stack<Command>();

	private List<CommandListener> listeners = new LinkedList<CommandListener>();

	public void execute(Command command) {
		// Only push if Command.execute does not throw an exception.
		command.execute();
		undoes.push(command);
		redoes.clear();

		for (CommandListener l : listeners) {
			l.commandExecuted(command);
		}
	}

	public void undo() {
		// Only pop and push if Command.undo does not throw an exception.
		Command c = undoes.peek();
		c.undo();
		undoes.pop();
		redoes.push(c);

		for (CommandListener l : listeners) {
			l.commandUndone(c);
		}
	}

	public void redo() {
		// Only pop and push Command.redo does not throw an exception.
		Command c = redoes.peek();
		c.redo();
		redoes.pop();
		undoes.push(c);

		for (CommandListener l : listeners) {
			l.commandRedone(c);
		}
	}

	public boolean canUndo() {
		return !undoes.empty();
	}

	public boolean canRedo() {
		return !redoes.empty();
	}

	public Stack<Command> getUndoes() {
		return undoes;
	}

	public Stack<Command> getRedoes() {
		return redoes;
	}

	// Listener support.

	public void addListener(CommandListener l) {
		listeners.add(l);
	}

	public void removeListener(CommandListener l) {
		listeners.remove(l);
	}

}
