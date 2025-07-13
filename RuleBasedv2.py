# Smart Home Automation Rule-Based System
# Modular design with separate components for sensors, devices, rules, and control

from abc import ABC, abstractmethod
from enum import Enum
from typing import Dict, List, Any, Optional
from dataclasses import dataclass
from datetime import datetime, time
import json
import tkinter as tk
from tkinter import ttk, messagebox, scrolledtext
import threading
import time as time_module

# ============= ENUMS AND DATA CLASSES =============

class DeviceType(Enum):
    LIGHT = "light"
    THERMOSTAT = "thermostat"
    SECURITY_CAMERA = "security_camera"
    DOOR_LOCK = "door_lock"
    WINDOW_BLINDS = "window_blinds"
    AIR_PURIFIER = "air_purifier"

class SensorType(Enum):
    TEMPERATURE = "temperature"
    HUMIDITY = "humidity"
    LIGHT = "light"
    MOTION = "motion"
    AIR_QUALITY = "air_quality"
    OCCUPANCY = "occupancy"

@dataclass
class SensorReading:
    sensor_type: SensorType
    value: Any
    timestamp: datetime
    location: str

@dataclass
class DeviceCommand:
    device_id: str
    action: str
    parameters: Dict[str, Any]
    timestamp: datetime

# ============= SENSOR MODULE =============

class Sensor(ABC):
    def __init__(self, sensor_id: str, location: str):
        self.sensor_id = sensor_id
        self.location = location
        self.last_reading = None
    
    @abstractmethod
    def read(self) -> SensorReading:
        pass

class TemperatureSensor(Sensor):
    def __init__(self, sensor_id: str, location: str, current_temp: float = 22.0):
        super().__init__(sensor_id, location)
        self.current_temp = current_temp
    
    def read(self) -> SensorReading:
        reading = SensorReading(
            sensor_type=SensorType.TEMPERATURE,
            value=self.current_temp,
            timestamp=datetime.now(),
            location=self.location
        )
        self.last_reading = reading
        return reading

class MotionSensor(Sensor):
    def __init__(self, sensor_id: str, location: str, motion_detected: bool = False):
        super().__init__(sensor_id, location)
        self.motion_detected = motion_detected
    
    def read(self) -> SensorReading:
        reading = SensorReading(
            sensor_type=SensorType.MOTION,
            value=self.motion_detected,
            timestamp=datetime.now(),
            location=self.location
        )
        self.last_reading = reading
        return reading

class LightSensor(Sensor):
    def __init__(self, sensor_id: str, location: str, lux_level: int = 300):
        super().__init__(sensor_id, location)
        self.lux_level = lux_level
    
    def read(self) -> SensorReading:
        reading = SensorReading(
            sensor_type=SensorType.LIGHT,
            value=self.lux_level,
            timestamp=datetime.now(),
            location=self.location
        )
        self.last_reading = reading
        return reading

class OccupancySensor(Sensor):
    def __init__(self, sensor_id: str, location: str, occupied: bool = False):
        super().__init__(sensor_id, location)
        self.occupied = occupied
    
    def read(self) -> SensorReading:
        reading = SensorReading(
            sensor_type=SensorType.OCCUPANCY,
            value=self.occupied,
            timestamp=datetime.now(),
            location=self.location
        )
        self.last_reading = reading
        return reading

# ============= DEVICE MODULE =============

class Device(ABC):
    def __init__(self, device_id: str, device_type: DeviceType, location: str):
        self.device_id = device_id
        self.device_type = device_type
        self.location = location
        self.is_on = False
        self.status = {}
    
    @abstractmethod
    def execute_command(self, command: DeviceCommand) -> bool:
        pass
    
    def get_status(self) -> Dict[str, Any]:
        return {
            "device_id": self.device_id,
            "type": self.device_type.value,
            "location": self.location,
            "is_on": self.is_on,
            "status": self.status
        }

class SmartLight(Device):
    def __init__(self, device_id: str, location: str):
        super().__init__(device_id, DeviceType.LIGHT, location)
        self.brightness = 100
        self.color = "white"
    
    def execute_command(self, command: DeviceCommand) -> bool:
        try:
            if command.action == "turn_on":
                self.is_on = True
                self.brightness = command.parameters.get("brightness", 100)
                self.color = command.parameters.get("color", "white")
            elif command.action == "turn_off":
                self.is_on = False
            elif command.action == "set_brightness":
                self.brightness = command.parameters.get("brightness", 100)
            elif command.action == "set_color":
                self.color = command.parameters.get("color", "white")
            
            self.status = {"brightness": self.brightness, "color": self.color}
            return True
        except Exception as e:
            print(f"Error executing command on {self.device_id}: {e}")
            return False

class Thermostat(Device):
    def __init__(self, device_id: str, location: str):
        super().__init__(device_id, DeviceType.THERMOSTAT, location)
        self.target_temp = 22.0
        self.mode = "auto"  # auto, heat, cool, off
    
    def execute_command(self, command: DeviceCommand) -> bool:
        try:
            if command.action == "set_temperature":
                self.target_temp = command.parameters.get("temperature", 22.0)
                self.is_on = True
            elif command.action == "set_mode":
                self.mode = command.parameters.get("mode", "auto")
            elif command.action == "turn_off":
                self.is_on = False
                self.mode = "off"
            
            self.status = {"target_temp": self.target_temp, "mode": self.mode}
            return True
        except Exception as e:
            print(f"Error executing command on {self.device_id}: {e}")
            return False

class SecurityCamera(Device):
    def __init__(self, device_id: str, location: str):
        super().__init__(device_id, DeviceType.SECURITY_CAMERA, location)
        self.recording = False
        self.motion_detection = True
    
    def execute_command(self, command: DeviceCommand) -> bool:
        try:
            if command.action == "start_recording":
                self.recording = True
                self.is_on = True
            elif command.action == "stop_recording":
                self.recording = False
            elif command.action == "enable_motion_detection":
                self.motion_detection = True
            elif command.action == "disable_motion_detection":
                self.motion_detection = False
            elif command.action == "turn_off":
                self.is_on = False
                self.recording = False
            
            self.status = {"recording": self.recording, "motion_detection": self.motion_detection}
            return True
        except Exception as e:
            print(f"Error executing command on {self.device_id}: {e}")
            return False

# ============= RULE ENGINE MODULE =============

class Rule(ABC):
    def __init__(self, rule_id: str, priority: int = 1):
        self.rule_id = rule_id
        self.priority = priority
        self.enabled = True
    
    @abstractmethod
    def evaluate(self, sensor_data: Dict[str, SensorReading], 
                 device_states: Dict[str, Device]) -> List[DeviceCommand]:
        pass

class TemperatureRule(Rule):
    def __init__(self, rule_id: str, target_temp: float, tolerance: float = 2.0):
        super().__init__(rule_id, priority=2)
        self.target_temp = target_temp
        self.tolerance = tolerance
    
    def evaluate(self, sensor_data: Dict[str, SensorReading], 
                 device_states: Dict[str, Device]) -> List[DeviceCommand]:
        commands = []
        
        # Find temperature sensors
        temp_readings = [reading for reading in sensor_data.values() 
                        if reading.sensor_type == SensorType.TEMPERATURE]
        
        for reading in temp_readings:
            current_temp = reading.value
            location = reading.location
            
            # Find thermostat in the same location
            thermostats = [device for device in device_states.values() 
                          if device.device_type == DeviceType.THERMOSTAT and 
                          device.location == location]
            
            for thermostat in thermostats:
                if abs(current_temp - self.target_temp) > self.tolerance:
                    command = DeviceCommand(
                        device_id=thermostat.device_id,
                        action="set_temperature",
                        parameters={"temperature": self.target_temp},
                        timestamp=datetime.now()
                    )
                    commands.append(command)
        
        return commands

class MotionLightRule(Rule):
    def __init__(self, rule_id: str):
        super().__init__(rule_id, priority=3)
    
    def evaluate(self, sensor_data: Dict[str, SensorReading], 
                 device_states: Dict[str, Device]) -> List[DeviceCommand]:
        commands = []
        
        # Find motion sensors
        motion_readings = [reading for reading in sensor_data.values() 
                          if reading.sensor_type == SensorType.MOTION]
        
        for reading in motion_readings:
            location = reading.location
            motion_detected = reading.value
            
            # Find lights in the same location
            lights = [device for device in device_states.values() 
                     if device.device_type == DeviceType.LIGHT and 
                     device.location == location]
            
            for light in lights:
                if motion_detected and not light.is_on:
                    command = DeviceCommand(
                        device_id=light.device_id,
                        action="turn_on",
                        parameters={"brightness": 80},
                        timestamp=datetime.now()
                    )
                    commands.append(command)
                elif not motion_detected and light.is_on:
                    # Turn off light after 5 minutes (simplified for demo)
                    command = DeviceCommand(
                        device_id=light.device_id,
                        action="turn_off",
                        parameters={},
                        timestamp=datetime.now()
                    )
                    commands.append(command)
        
        return commands

class SecurityRule(Rule):
    def __init__(self, rule_id: str):
        super().__init__(rule_id, priority=1)  # High priority
    
    def evaluate(self, sensor_data: Dict[str, SensorReading], 
                 device_states: Dict[str, Device]) -> List[DeviceCommand]:
        commands = []
        
        # Check occupancy sensors
        occupancy_readings = [reading for reading in sensor_data.values() 
                             if reading.sensor_type == SensorType.OCCUPANCY]
        
        for reading in occupancy_readings:
            location = reading.location
            occupied = reading.value
            
            # Find security cameras in the same location
            cameras = [device for device in device_states.values() 
                      if device.device_type == DeviceType.SECURITY_CAMERA and 
                      device.location == location]
            
            for camera in cameras:
                if not occupied and not camera.recording:
                    # Start recording when area is unoccupied
                    command = DeviceCommand(
                        device_id=camera.device_id,
                        action="start_recording",
                        parameters={},
                        timestamp=datetime.now()
                    )
                    commands.append(command)
        
        return commands

class AmbientLightRule(Rule):
    def __init__(self, rule_id: str, low_light_threshold: int = 200):
        super().__init__(rule_id, priority=2)
        self.low_light_threshold = low_light_threshold
    
    def evaluate(self, sensor_data: Dict[str, SensorReading], 
                 device_states: Dict[str, Device]) -> List[DeviceCommand]:
        commands = []
        
        # Find light sensors
        light_readings = [reading for reading in sensor_data.values() 
                         if reading.sensor_type == SensorType.LIGHT]
        
        for reading in light_readings:
            location = reading.location
            lux_level = reading.value
            
            # Find lights in the same location
            lights = [device for device in device_states.values() 
                     if device.device_type == DeviceType.LIGHT and 
                     device.location == location]
            
            for light in lights:
                if lux_level < self.low_light_threshold and not light.is_on:
                    # Turn on light with appropriate brightness
                    brightness = max(30, min(100, 100 - (lux_level / 10)))
                    command = DeviceCommand(
                        device_id=light.device_id,
                        action="turn_on",
                        parameters={"brightness": int(brightness)},
                        timestamp=datetime.now()
                    )
                    commands.append(command)
        
        return commands

# ============= RULE ENGINE =============

class RuleEngine:
    def __init__(self):
        self.rules: List[Rule] = []
    
    def add_rule(self, rule: Rule):
        self.rules.append(rule)
        # Sort by priority (higher priority first)
        self.rules.sort(key=lambda r: r.priority, reverse=True)
    
    def remove_rule(self, rule_id: str):
        self.rules = [rule for rule in self.rules if rule.rule_id != rule_id]
    
    def evaluate_all(self, sensor_data: Dict[str, SensorReading], 
                     device_states: Dict[str, Device]) -> List[DeviceCommand]:
        all_commands = []
        
        for rule in self.rules:
            if rule.enabled:
                try:
                    commands = rule.evaluate(sensor_data, device_states)
                    all_commands.extend(commands)
                except Exception as e:
                    print(f"Error evaluating rule {rule.rule_id}: {e}")
        
        return all_commands

# ============= MAIN SYSTEM CONTROLLER =============

class SmartHomeController:
    def __init__(self):
        self.sensors: Dict[str, Sensor] = {}
        self.devices: Dict[str, Device] = {}
        self.rule_engine = RuleEngine()
        self.sensor_data: Dict[str, SensorReading] = {}
        self.command_history: List[DeviceCommand] = []
    
    def add_sensor(self, sensor: Sensor):
        self.sensors[sensor.sensor_id] = sensor
    
    def add_device(self, device: Device):
        self.devices[device.device_id] = device
    
    def add_rule(self, rule: Rule):
        self.rule_engine.add_rule(rule)
    
    def read_all_sensors(self):
        """Read data from all sensors"""
        for sensor_id, sensor in self.sensors.items():
            reading = sensor.read()
            self.sensor_data[sensor_id] = reading
    
    def execute_commands(self, commands: List[DeviceCommand]):
        """Execute device commands"""
        for command in commands:
            if command.device_id in self.devices:
                device = self.devices[command.device_id]
                success = device.execute_command(command)
                if success:
                    self.command_history.append(command)
                    print(f"✓ Executed: {command.action} on {command.device_id}")
                else:
                    print(f"✗ Failed to execute: {command.action} on {command.device_id}")
    
    def run_automation_cycle(self):
        """Main automation cycle"""
        print("\n=== Starting Automation Cycle ===")
        
        # 1. Read all sensors
        self.read_all_sensors()
        print(f"Read {len(self.sensor_data)} sensor readings")
        
        # 2. Evaluate rules
        commands = self.rule_engine.evaluate_all(self.sensor_data, self.devices)
        print(f"Generated {len(commands)} commands")
        
        # 3. Execute commands
        if commands:
            self.execute_commands(commands)
        
        print("=== Automation Cycle Complete ===\n")
    
    def get_system_status(self) -> Dict[str, Any]:
        """Get current system status"""
        return {
            "sensors": {sid: sensor.last_reading.__dict__ if sensor.last_reading else None 
                       for sid, sensor in self.sensors.items()},
            "devices": {did: device.get_status() for did, device in self.devices.items()},
            "rules": len(self.rule_engine.rules),
            "last_commands": [cmd.__dict__ for cmd in self.command_history[-5:]]
        }

# ============= GUI MODULE =============

class SmartHomeGUI:
    def __init__(self, root):
        self.root = root
        self.root.title("Smart Home Automation System")
        self.root.geometry("1200x800")
        self.root.configure(bg='#2c3e50')
        
        # Initialize the smart home controller
        self.controller = SmartHomeController()
        self.setup_demo_system()
        
        # GUI state
        self.is_running = False
        self.auto_cycle_thread = None
        
        # Create GUI elements
        self.setup_styles()
        self.create_widgets()
        
        # Start periodic updates
        self.update_display()
    
    def setup_styles(self):
        """Setup modern styling"""
        style = ttk.Style()
        style.theme_use('clam')
        
        # Configure colors
        style.configure('Title.TLabel', font=('Segoe UI', 16, 'bold'), background='#2c3e50', foreground='white')
        style.configure('Header.TLabel', font=('Segoe UI', 12, 'bold'), background='#34495e', foreground='white')
        style.configure('Modern.TButton', font=('Segoe UI', 10), padding=8)
        style.configure('Status.TLabel', font=('Segoe UI', 9), background='#2c3e50', foreground='#ecf0f1')
        style.configure('Card.TFrame', background='#34495e', relief='raised', borderwidth=2)
    
    def create_widgets(self):
        """Create all GUI widgets"""
        # Main container
        main_frame = ttk.Frame(self.root, style='Card.TFrame')
        main_frame.pack(fill=tk.BOTH, expand=True, padx=20, pady=20)
        
        # Title
        title_label = ttk.Label(main_frame, text="🏠 Smart Home Automation System", style='Title.TLabel')
        title_label.pack(pady=(20, 10))
        
        # Create notebook for tabs
        self.notebook = ttk.Notebook(main_frame)
        self.notebook.pack(fill=tk.BOTH, expand=True, padx=20, pady=20)
        
        # Create tabs
        self.create_dashboard_tab()
        self.create_sensors_tab()
        self.create_devices_tab()
        self.create_rules_tab()
        self.create_automation_tab()
        self.create_logs_tab()
    
    def create_dashboard_tab(self):
        """Create the main dashboard tab"""
        dashboard_frame = ttk.Frame(self.notebook)
        self.notebook.add(dashboard_frame, text="📊 Dashboard")
        
        # Status overview
        status_frame = ttk.LabelFrame(dashboard_frame, text="System Status", padding=10)
        status_frame.pack(fill=tk.X, padx=10, pady=10)
        
        # Status labels
        self.status_labels = {}
        status_items = [
            ("Active Sensors", "0"),
            ("Active Devices", "0"),
            ("Active Rules", "0"),
            ("Last Cycle", "Never")
        ]
        
        for i, (label, value) in enumerate(status_items):
            row = i // 2
            col = i % 2
            
            frame = ttk.Frame(status_frame)
            frame.grid(row=row, column=col, padx=10, pady=5, sticky='ew')
            
            ttk.Label(frame, text=f"{label}:", style='Header.TLabel').pack(anchor='w')
            value_label = ttk.Label(frame, text=value, style='Status.TLabel')
            value_label.pack(anchor='w')
            self.status_labels[label] = value_label
        
        # Control buttons
        control_frame = ttk.Frame(dashboard_frame)
        control_frame.pack(fill=tk.X, padx=10, pady=10)
        
        self.start_button = ttk.Button(control_frame, text="▶ Start Automation", 
                                      command=self.start_automation, style='Modern.TButton')
        self.start_button.pack(side=tk.LEFT, padx=5)
        
        self.stop_button = ttk.Button(control_frame, text="⏹ Stop Automation", 
                                     command=self.stop_automation, style='Modern.TButton')
        self.stop_button.pack(side=tk.LEFT, padx=5)
        
        self.run_cycle_button = ttk.Button(control_frame, text="🔄 Run Single Cycle", 
                                          command=self.run_single_cycle, style='Modern.TButton')
        self.run_cycle_button.pack(side=tk.LEFT, padx=5)
        
        # Real-time display
        display_frame = ttk.LabelFrame(dashboard_frame, text="Real-time Data", padding=10)
        display_frame.pack(fill=tk.BOTH, expand=True, padx=10, pady=10)
        
        # Create display areas
        self.create_sensor_display(display_frame)
        self.create_device_display(display_frame)
    
    def create_sensor_display(self, parent):
        """Create sensor data display"""
        sensor_frame = ttk.LabelFrame(parent, text="📡 Sensor Readings", padding=10)
        sensor_frame.pack(fill=tk.BOTH, expand=True, side=tk.LEFT, padx=(0, 5))
        
        # Sensor readings text area
        self.sensor_text = scrolledtext.ScrolledText(sensor_frame, height=15, width=40, 
                                                   font=('Consolas', 9), bg='#2c3e50', fg='#ecf0f1')
        self.sensor_text.pack(fill=tk.BOTH, expand=True)
    
    def create_device_display(self, parent):
        """Create device status display"""
        device_frame = ttk.LabelFrame(parent, text="💡 Device Status", padding=10)
        device_frame.pack(fill=tk.BOTH, expand=True, side=tk.RIGHT, padx=(5, 0))
        
        # Device status text area
        self.device_text = scrolledtext.ScrolledText(device_frame, height=15, width=40, 
                                                   font=('Consolas', 9), bg='#2c3e50', fg='#ecf0f1')
        self.device_text.pack(fill=tk.BOTH, expand=True)
    
    def create_sensors_tab(self):
        """Create sensors management tab"""
        sensors_frame = ttk.Frame(self.notebook)
        self.notebook.add(sensors_frame, text="📡 Sensors")
        
        # Add sensor section
        add_frame = ttk.LabelFrame(sensors_frame, text="Add New Sensor", padding=10)
        add_frame.pack(fill=tk.X, padx=10, pady=10)
        
        # Sensor type selection
        ttk.Label(add_frame, text="Sensor Type:").grid(row=0, column=0, padx=5, pady=5, sticky='w')
        self.sensor_type_var = tk.StringVar(value="temperature")
        sensor_types = [("Temperature", "temperature"), ("Motion", "motion"), 
                       ("Light", "light"), ("Occupancy", "occupancy")]
        
        for i, (text, value) in enumerate(sensor_types):
            ttk.Radiobutton(add_frame, text=text, variable=self.sensor_type_var, 
                           value=value).grid(row=0, column=i+1, padx=5, pady=5)
        
        # Sensor details
        ttk.Label(add_frame, text="Sensor ID:").grid(row=1, column=0, padx=5, pady=5, sticky='w')
        self.sensor_id_entry = ttk.Entry(add_frame, width=20)
        self.sensor_id_entry.grid(row=1, column=1, padx=5, pady=5, sticky='w')
        
        ttk.Label(add_frame, text="Location:").grid(row=1, column=2, padx=5, pady=5, sticky='w')
        self.sensor_location_entry = ttk.Entry(add_frame, width=20)
        self.sensor_location_entry.grid(row=1, column=3, padx=5, pady=5, sticky='w')
        
        # Add button
        ttk.Button(add_frame, text="Add Sensor", command=self.add_sensor, 
                  style='Modern.TButton').grid(row=1, column=4, padx=10, pady=5)
        
        # Sensor list
        list_frame = ttk.LabelFrame(sensors_frame, text="Current Sensors", padding=10)
        list_frame.pack(fill=tk.BOTH, expand=True, padx=10, pady=10)
        
        # Treeview for sensors
        columns = ('ID', 'Type', 'Location', 'Status', 'Last Reading')
        self.sensor_tree = ttk.Treeview(list_frame, columns=columns, show='headings', height=10)
        
        for col in columns:
            self.sensor_tree.heading(col, text=col)
            self.sensor_tree.column(col, width=120)
        
        self.sensor_tree.pack(fill=tk.BOTH, expand=True)
        
        # Buttons for sensor management
        button_frame = ttk.Frame(list_frame)
        button_frame.pack(fill=tk.X, pady=10)
        
        ttk.Button(button_frame, text="Remove Selected", command=self.remove_sensor, 
                  style='Modern.TButton').pack(side=tk.LEFT, padx=5)
        ttk.Button(button_frame, text="Update Readings", command=self.update_sensor_readings, 
                  style='Modern.TButton').pack(side=tk.LEFT, padx=5)
    
    def create_devices_tab(self):
        """Create devices management tab"""
        devices_frame = ttk.Frame(self.notebook)
        self.notebook.add(devices_frame, text="💡 Devices")
        
        # Add device section
        add_frame = ttk.LabelFrame(devices_frame, text="Add New Device", padding=10)
        add_frame.pack(fill=tk.X, padx=10, pady=10)
        
        # Device type selection
        ttk.Label(add_frame, text="Device Type:").grid(row=0, column=0, padx=5, pady=5, sticky='w')
        self.device_type_var = tk.StringVar(value="light")
        device_types = [("Smart Light", "light"), ("Thermostat", "thermostat"), 
                       ("Security Camera", "security_camera")]
        
        for i, (text, value) in enumerate(device_types):
            ttk.Radiobutton(add_frame, text=text, variable=self.device_type_var, 
                           value=value).grid(row=0, column=i+1, padx=5, pady=5)
        
        # Device details
        ttk.Label(add_frame, text="Device ID:").grid(row=1, column=0, padx=5, pady=5, sticky='w')
        self.device_id_entry = ttk.Entry(add_frame, width=20)
        self.device_id_entry.grid(row=1, column=1, padx=5, pady=5, sticky='w')
        
        ttk.Label(add_frame, text="Location:").grid(row=1, column=2, padx=5, pady=5, sticky='w')
        self.device_location_entry = ttk.Entry(add_frame, width=20)
        self.device_location_entry.grid(row=1, column=3, padx=5, pady=5, sticky='w')
        
        # Add button
        ttk.Button(add_frame, text="Add Device", command=self.add_device, 
                  style='Modern.TButton').grid(row=1, column=4, padx=10, pady=5)
        
        # Device list
        list_frame = ttk.LabelFrame(devices_frame, text="Current Devices", padding=10)
        list_frame.pack(fill=tk.BOTH, expand=True, padx=10, pady=10)
        
        # Treeview for devices
        columns = ('ID', 'Type', 'Location', 'Status', 'Details')
        self.device_tree = ttk.Treeview(list_frame, columns=columns, show='headings', height=10)
        
        for col in columns:
            self.device_tree.heading(col, text=col)
            self.device_tree.column(col, width=120)
        
        self.device_tree.pack(fill=tk.BOTH, expand=True)
        
        # Buttons for device management
        button_frame = ttk.Frame(list_frame)
        button_frame.pack(fill=tk.X, pady=10)
        
        ttk.Button(button_frame, text="Remove Selected", command=self.remove_device, 
                  style='Modern.TButton').pack(side=tk.LEFT, padx=5)
        ttk.Button(button_frame, text="Control Device", command=self.control_device, 
                  style='Modern.TButton').pack(side=tk.LEFT, padx=5)
    
    def create_rules_tab(self):
        """Create rules management tab"""
        rules_frame = ttk.Frame(self.notebook)
        self.notebook.add(rules_frame, text="⚙️ Rules")
        
        # Add rule section
        add_frame = ttk.LabelFrame(rules_frame, text="Add New Rule", padding=10)
        add_frame.pack(fill=tk.X, padx=10, pady=10)
        
        # Rule type selection
        ttk.Label(add_frame, text="Rule Type:").grid(row=0, column=0, padx=5, pady=5, sticky='w')
        self.rule_type_var = tk.StringVar(value="temperature")
        rule_types = [("Temperature Control", "temperature"), ("Motion Lights", "motion"), 
                     ("Security Monitoring", "security"), ("Ambient Lighting", "ambient")]
        
        for i, (text, value) in enumerate(rule_types):
            ttk.Radiobutton(add_frame, text=text, variable=self.rule_type_var, 
                           value=value).grid(row=0, column=i+1, padx=5, pady=5)
        
        # Rule parameters
        ttk.Label(add_frame, text="Rule ID:").grid(row=1, column=0, padx=5, pady=5, sticky='w')
        self.rule_id_entry = ttk.Entry(add_frame, width=20)
        self.rule_id_entry.grid(row=1, column=1, padx=5, pady=5, sticky='w')
        
        ttk.Label(add_frame, text="Priority:").grid(row=1, column=2, padx=5, pady=5, sticky='w')
        self.rule_priority_var = tk.StringVar(value="2")
        priority_combo = ttk.Combobox(add_frame, textvariable=self.rule_priority_var, 
                                     values=["1", "2", "3"], width=10)
        priority_combo.grid(row=1, column=3, padx=5, pady=5, sticky='w')
        
        # Add button
        ttk.Button(add_frame, text="Add Rule", command=self.add_rule, 
                  style='Modern.TButton').grid(row=1, column=4, padx=10, pady=5)
        
        # Rule list
        list_frame = ttk.LabelFrame(rules_frame, text="Current Rules", padding=10)
        list_frame.pack(fill=tk.BOTH, expand=True, padx=10, pady=10)
        
        # Treeview for rules
        columns = ('ID', 'Type', 'Priority', 'Status', 'Last Triggered')
        self.rule_tree = ttk.Treeview(list_frame, columns=columns, show='headings', height=10)
        
        for col in columns:
            self.rule_tree.heading(col, text=col)
            self.rule_tree.column(col, width=120)
        
        self.rule_tree.pack(fill=tk.BOTH, expand=True)
        
        # Buttons for rule management
        button_frame = ttk.Frame(list_frame)
        button_frame.pack(fill=tk.X, pady=10)
        
        ttk.Button(button_frame, text="Remove Selected", command=self.remove_rule, 
                  style='Modern.TButton').pack(side=tk.LEFT, padx=5)
        ttk.Button(button_frame, text="Enable/Disable", command=self.toggle_rule, 
                  style='Modern.TButton').pack(side=tk.LEFT, padx=5)
    
    def create_automation_tab(self):
        """Create automation control tab"""
        automation_frame = ttk.Frame(self.notebook)
        self.notebook.add(automation_frame, text="🤖 Automation")
        
        # Automation controls
        control_frame = ttk.LabelFrame(automation_frame, text="Automation Controls", padding=10)
        control_frame.pack(fill=tk.X, padx=10, pady=10)
        
        # Cycle interval
        ttk.Label(control_frame, text="Cycle Interval (seconds):").grid(row=0, column=0, padx=5, pady=5, sticky='w')
        self.cycle_interval_var = tk.StringVar(value="5")
        self.cycle_interval_entry = ttk.Entry(control_frame, textvariable=self.cycle_interval_var, width=10)
        self.cycle_interval_entry.grid(row=0, column=1, padx=5, pady=5, sticky='w')
        
        # Control buttons
        button_frame = ttk.Frame(control_frame)
        button_frame.grid(row=0, column=2, padx=20, pady=5)
        
        self.auto_start_button = ttk.Button(button_frame, text="▶ Start Auto", 
                                           command=self.start_auto_cycle, style='Modern.TButton')
        self.auto_start_button.pack(side=tk.LEFT, padx=5)
        
        self.auto_stop_button = ttk.Button(button_frame, text="⏹ Stop Auto", 
                                          command=self.stop_auto_cycle, style='Modern.TButton')
        self.auto_stop_button.pack(side=tk.LEFT, padx=5)
        
        # Manual controls
        manual_frame = ttk.LabelFrame(automation_frame, text="Manual Controls", padding=10)
        manual_frame.pack(fill=tk.X, padx=10, pady=10)
        
        ttk.Button(manual_frame, text="🔄 Run Single Cycle", command=self.run_single_cycle, 
                  style='Modern.TButton').pack(side=tk.LEFT, padx=5, pady=5)
        
        ttk.Button(manual_frame, text="📊 Show Status", command=self.show_system_status, 
                  style='Modern.TButton').pack(side=tk.LEFT, padx=5, pady=5)
        
        ttk.Button(manual_frame, text="🔄 Reset System", command=self.reset_system, 
                  style='Modern.TButton').pack(side=tk.LEFT, padx=5, pady=5)
    
    def create_logs_tab(self):
        """Create logs and history tab"""
        logs_frame = ttk.Frame(self.notebook)
        self.notebook.add(logs_frame, text="📋 Logs")
        
        # Log display
        log_frame = ttk.LabelFrame(logs_frame, text="System Logs", padding=10)
        log_frame.pack(fill=tk.BOTH, expand=True, padx=10, pady=10)
        
        # Log text area
        self.log_text = scrolledtext.ScrolledText(log_frame, height=20, width=80, 
                                                font=('Consolas', 9), bg='#2c3e50', fg='#ecf0f1')
        self.log_text.pack(fill=tk.BOTH, expand=True)
        
        # Log controls
        control_frame = ttk.Frame(log_frame)
        control_frame.pack(fill=tk.X, pady=10)
        
        ttk.Button(control_frame, text="Clear Logs", command=self.clear_logs, 
                  style='Modern.TButton').pack(side=tk.LEFT, padx=5)
        ttk.Button(control_frame, text="Export Logs", command=self.export_logs, 
                  style='Modern.TButton').pack(side=tk.LEFT, padx=5)
    
    def setup_demo_system(self):
        """Setup a demo system with sensors, devices, and rules"""
        # Add demo sensors
        self.controller.add_sensor(TemperatureSensor("temp_living", "living_room", 22.0))
        self.controller.add_sensor(MotionSensor("motion_living", "living_room", False))
        self.controller.add_sensor(LightSensor("light_living", "living_room", 300))
        self.controller.add_sensor(OccupancySensor("occupancy_living", "living_room", True))
        
        # Add demo devices
        self.controller.add_device(SmartLight("light_living_main", "living_room"))
        self.controller.add_device(Thermostat("thermostat_main", "living_room"))
        self.controller.add_device(SecurityCamera("camera_living", "living_room"))
        
        # Add demo rules
        self.controller.add_rule(TemperatureRule("temp_control", target_temp=22.0))
        self.controller.add_rule(MotionLightRule("motion_lights"))
        self.controller.add_rule(SecurityRule("security_monitoring"))
        self.controller.add_rule(AmbientLightRule("ambient_lighting"))
    
    def update_display(self):
        """Update all displays with current data"""
        self.update_sensor_display()
        self.update_device_display()
        self.update_status_labels()
        self.update_trees()
        
        # Schedule next update
        self.root.after(1000, self.update_display)
    
    def update_sensor_display(self):
        """Update sensor readings display"""
        self.sensor_text.delete('1.0', tk.END)
        
        for sensor_id, sensor in self.controller.sensors.items():
            if sensor.last_reading:
                reading = sensor.last_reading
                self.sensor_text.insert(tk.END, f"📡 {sensor_id}\n")
                self.sensor_text.insert(tk.END, f"   Type: {reading.sensor_type.value}\n")
                self.sensor_text.insert(tk.END, f"   Value: {reading.value}\n")
                self.sensor_text.insert(tk.END, f"   Location: {reading.location}\n")
                self.sensor_text.insert(tk.END, f"   Time: {reading.timestamp.strftime('%H:%M:%S')}\n")
                self.sensor_text.insert(tk.END, "-" * 40 + "\n")
    
    def update_device_display(self):
        """Update device status display"""
        self.device_text.delete('1.0', tk.END)
        
        for device_id, device in self.controller.devices.items():
            status = device.get_status()
            self.device_text.insert(tk.END, f"💡 {device_id}\n")
            self.device_text.insert(tk.END, f"   Type: {status['type']}\n")
            self.device_text.insert(tk.END, f"   Location: {status['location']}\n")
            self.device_text.insert(tk.END, f"   Status: {'🟢 ON' if status['is_on'] else '🔴 OFF'}\n")
            if status['status']:
                for key, value in status['status'].items():
                    self.device_text.insert(tk.END, f"   {key}: {value}\n")
            self.device_text.insert(tk.END, "-" * 40 + "\n")
    
    def update_status_labels(self):
        """Update status labels"""
        active_sensors = len([s for s in self.controller.sensors.values() if s.last_reading])
        active_devices = len([d for d in self.controller.devices.values() if d.is_on])
        active_rules = len([r for r in self.controller.rule_engine.rules if r.enabled])
        
        self.status_labels["Active Sensors"].config(text=str(active_sensors))
        self.status_labels["Active Devices"].config(text=str(active_devices))
        self.status_labels["Active Rules"].config(text=str(active_rules))
        
        if hasattr(self, 'last_cycle_time'):
            self.status_labels["Last Cycle"].config(text=self.last_cycle_time)
        else:
            self.status_labels["Last Cycle"].config(text="Never")
    
    def update_trees(self):
        """Update treeviews"""
        # Update sensor tree
        for item in self.sensor_tree.get_children():
            self.sensor_tree.delete(item)
        
        for sensor_id, sensor in self.controller.sensors.items():
            status = "Active" if sensor.last_reading else "Inactive"
            last_reading = sensor.last_reading.value if sensor.last_reading else "N/A"
            self.sensor_tree.insert('', 'end', values=(
                sensor_id, sensor.__class__.__name__, sensor.location, status, last_reading
            ))
        
        # Update device tree
        for item in self.device_tree.get_children():
            self.device_tree.delete(item)
        
        for device_id, device in self.controller.devices.items():
            status = "ON" if device.is_on else "OFF"
            details = str(device.get_status()['status'])
            self.device_tree.insert('', 'end', values=(
                device_id, device.device_type.value, device.location, status, details
            ))
        
        # Update rule tree
        for item in self.rule_tree.get_children():
            self.rule_tree.delete(item)
        
        for rule in self.controller.rule_engine.rules:
            status = "Enabled" if rule.enabled else "Disabled"
            self.rule_tree.insert('', 'end', values=(
                rule.rule_id, rule.__class__.__name__, rule.priority, status, "N/A"
            ))
    
    def log_message(self, message):
        """Add message to log"""
        timestamp = datetime.now().strftime('%H:%M:%S')
        self.log_text.insert(tk.END, f"[{timestamp}] {message}\n")
        self.log_text.see(tk.END)
    
    def start_automation(self):
        """Start automation system"""
        self.is_running = True
        self.log_message("🚀 Automation system started")
        self.start_button.config(state='disabled')
        self.stop_button.config(state='normal')
    
    def stop_automation(self):
        """Stop automation system"""
        self.is_running = False
        self.log_message("⏹ Automation system stopped")
        self.start_button.config(state='normal')
        self.stop_button.config(state='disabled')
    
    def run_single_cycle(self):
        """Run a single automation cycle"""
        self.controller.run_automation_cycle()
        self.last_cycle_time = datetime.now().strftime('%H:%M:%S')
        self.log_message("🔄 Single automation cycle completed")
    
    def start_auto_cycle(self):
        """Start automatic cycling"""
        self.is_running = True
        self.auto_start_button.config(state='disabled')
        self.auto_stop_button.config(state='normal')
        self.log_message("🔄 Auto-cycling started")
        
        def auto_cycle():
            while self.is_running:
                self.controller.run_automation_cycle()
                self.last_cycle_time = datetime.now().strftime('%H:%M:%S')
                self.log_message("🔄 Auto-cycle completed")
                time_module.sleep(int(self.cycle_interval_var.get()))
        
        self.auto_cycle_thread = threading.Thread(target=auto_cycle, daemon=True)
        self.auto_cycle_thread.start()
    
    def stop_auto_cycle(self):
        """Stop automatic cycling"""
        self.is_running = False
        self.auto_start_button.config(state='normal')
        self.auto_stop_button.config(state='disabled')
        self.log_message("⏹ Auto-cycling stopped")
    
    def add_sensor(self):
        """Add a new sensor"""
        sensor_id = self.sensor_id_entry.get()
        location = self.sensor_location_entry.get()
        sensor_type = self.sensor_type_var.get()
        
        if not sensor_id or not location:
            messagebox.showerror("Error", "Please enter sensor ID and location")
            return
        
        # Create sensor based on type
        if sensor_type == "temperature":
            sensor = TemperatureSensor(sensor_id, location)
        elif sensor_type == "motion":
            sensor = MotionSensor(sensor_id, location)
        elif sensor_type == "light":
            sensor = LightSensor(sensor_id, location)
        elif sensor_type == "occupancy":
            sensor = OccupancySensor(sensor_id, location)
        
        self.controller.add_sensor(sensor)
        self.log_message(f"📡 Added sensor: {sensor_id} ({sensor_type}) at {location}")
        
        # Clear entries
        self.sensor_id_entry.delete(0, tk.END)
        self.sensor_location_entry.delete(0, tk.END)
    
    def add_device(self):
        """Add a new device"""
        device_id = self.device_id_entry.get()
        location = self.device_location_entry.get()
        device_type = self.device_type_var.get()
        
        if not device_id or not location:
            messagebox.showerror("Error", "Please enter device ID and location")
            return
        
        # Create device based on type
        if device_type == "light":
            device = SmartLight(device_id, location)
        elif device_type == "thermostat":
            device = Thermostat(device_id, location)
        elif device_type == "security_camera":
            device = SecurityCamera(device_id, location)
        
        self.controller.add_device(device)
        self.log_message(f"💡 Added device: {device_id} ({device_type}) at {location}")
        
        # Clear entries
        self.device_id_entry.delete(0, tk.END)
        self.device_location_entry.delete(0, tk.END)
    
    def add_rule(self):
        """Add a new rule"""
        rule_id = self.rule_id_entry.get()
        rule_type = self.rule_type_var.get()
        priority = int(self.rule_priority_var.get())
        
        if not rule_id:
            messagebox.showerror("Error", "Please enter rule ID")
            return
        
        # Create rule based on type
        if rule_type == "temperature":
            rule = TemperatureRule(rule_id, target_temp=22.0)
        elif rule_type == "motion":
            rule = MotionLightRule(rule_id)
        elif rule_type == "security":
            rule = SecurityRule(rule_id)
        elif rule_type == "ambient":
            rule = AmbientLightRule(rule_id)
        
        rule.priority = priority
        self.controller.add_rule(rule)
        self.log_message(f"⚙️ Added rule: {rule_id} ({rule_type}) with priority {priority}")
        
        # Clear entries
        self.rule_id_entry.delete(0, tk.END)
    
    def remove_sensor(self):
        """Remove selected sensor"""
        selection = self.sensor_tree.selection()
        if not selection:
            messagebox.showwarning("Warning", "Please select a sensor to remove")
            return
        
        item = self.sensor_tree.item(selection[0])
        sensor_id = item['values'][0]
        
        if sensor_id in self.controller.sensors:
            del self.controller.sensors[sensor_id]
            self.log_message(f"📡 Removed sensor: {sensor_id}")
    
    def remove_device(self):
        """Remove selected device"""
        selection = self.device_tree.selection()
        if not selection:
            messagebox.showwarning("Warning", "Please select a device to remove")
            return
        
        item = self.device_tree.item(selection[0])
        device_id = item['values'][0]
        
        if device_id in self.controller.devices:
            del self.controller.devices[device_id]
            self.log_message(f"💡 Removed device: {device_id}")
    
    def remove_rule(self):
        """Remove selected rule"""
        selection = self.rule_tree.selection()
        if not selection:
            messagebox.showwarning("Warning", "Please select a rule to remove")
            return
        
        item = self.rule_tree.item(selection[0])
        rule_id = item['values'][0]
        
        self.controller.rule_engine.remove_rule(rule_id)
        self.log_message(f"⚙️ Removed rule: {rule_id}")
    
    def update_sensor_readings(self):
        """Update sensor readings"""
        self.controller.read_all_sensors()
        self.log_message("📡 Updated all sensor readings")
    
    def control_device(self):
        """Control selected device"""
        selection = self.device_tree.selection()
        if not selection:
            messagebox.showwarning("Warning", "Please select a device to control")
            return
        
        item = self.device_tree.item(selection[0])
        device_id = item['values'][0]
        
        if device_id in self.controller.devices:
            device = self.controller.devices[device_id]
            
            # Create control dialog
            control_window = tk.Toplevel(self.root)
            control_window.title(f"Control {device_id}")
            control_window.geometry("300x200")
            
            ttk.Label(control_window, text=f"Control {device_id}").pack(pady=10)
            
            if device.device_type == DeviceType.LIGHT:
                ttk.Button(control_window, text="Turn On", 
                          command=lambda: self.execute_device_command(device_id, "turn_on", {})).pack(pady=5)
                ttk.Button(control_window, text="Turn Off", 
                          command=lambda: self.execute_device_command(device_id, "turn_off", {})).pack(pady=5)
            elif device.device_type == DeviceType.THERMOSTAT:
                ttk.Button(control_window, text="Set to 20°C", 
                          command=lambda: self.execute_device_command(device_id, "set_temperature", {"temperature": 20.0})).pack(pady=5)
                ttk.Button(control_window, text="Set to 25°C", 
                          command=lambda: self.execute_device_command(device_id, "set_temperature", {"temperature": 25.0})).pack(pady=5)
    
    def execute_device_command(self, device_id, action, parameters):
        """Execute a device command"""
        command = DeviceCommand(device_id, action, parameters, datetime.now())
        if device_id in self.controller.devices:
            device = self.controller.devices[device_id]
            success = device.execute_command(command)
            if success:
                self.log_message(f"✅ Executed {action} on {device_id}")
            else:
                self.log_message(f"❌ Failed to execute {action} on {device_id}")
    
    def toggle_rule(self):
        """Toggle rule enabled/disabled"""
        selection = self.rule_tree.selection()
        if not selection:
            messagebox.showwarning("Warning", "Please select a rule to toggle")
            return
        
        item = self.rule_tree.item(selection[0])
        rule_id = item['values'][0]
        
        for rule in self.controller.rule_engine.rules:
            if rule.rule_id == rule_id:
                rule.enabled = not rule.enabled
                status = "enabled" if rule.enabled else "disabled"
                self.log_message(f"⚙️ {rule_id} {status}")
                break
    
    def show_system_status(self):
        """Show detailed system status"""
        status = self.controller.get_system_status()
        
        status_window = tk.Toplevel(self.root)
        status_window.title("System Status")
        status_window.geometry("600x400")
        
        text = scrolledtext.ScrolledText(status_window, font=('Consolas', 10))
        text.pack(fill=tk.BOTH, expand=True, padx=10, pady=10)
        
        text.insert(tk.END, "=== SMART HOME SYSTEM STATUS ===\n\n")
        text.insert(tk.END, f"Sensors: {len(self.controller.sensors)}\n")
        text.insert(tk.END, f"Devices: {len(self.controller.devices)}\n")
        text.insert(tk.END, f"Rules: {len(self.controller.rule_engine.rules)}\n")
        text.insert(tk.END, f"Last Commands: {len(status['last_commands'])}\n\n")
        
        text.insert(tk.END, "=== DETAILED STATUS ===\n")
        text.insert(tk.END, json.dumps(status, indent=2, default=str))
    
    def reset_system(self):
        """Reset the entire system"""
        if messagebox.askyesno("Reset System", "Are you sure you want to reset the entire system?"):
            self.controller = SmartHomeController()
            self.setup_demo_system()
            self.log_message("🔄 System reset completed")
    
    def clear_logs(self):
        """Clear log display"""
        self.log_text.delete('1.0', tk.END)
        self.log_message("📋 Logs cleared")
    
    def export_logs(self):
        """Export logs to file"""
        logs = self.log_text.get('1.0', tk.END)
        try:
            with open('smart_home_logs.txt', 'w') as f:
                f.write(logs)
            self.log_message("📄 Logs exported to smart_home_logs.txt")
        except Exception as e:
            messagebox.showerror("Error", f"Failed to export logs: {e}")

def main():
    """Main function to run the GUI"""
    root = tk.Tk()
    app = SmartHomeGUI(root)
    
    # Center the window
    root.update_idletasks()
    width = root.winfo_width()
    height = root.winfo_height()
    x = (root.winfo_screenwidth() // 2) - (width // 2)
    y = (root.winfo_screenheight() // 2) - (height // 2)
    root.geometry(f'{width}x{height}+{x}+{y}')
    
    # Set minimum window size
    root.minsize(1000, 700)
    
    # Handle window closing
    def on_closing():
        if app.is_running:
            app.stop_automation()
        root.quit()
        root.destroy()
    
    root.protocol("WM_DELETE_WINDOW", on_closing)
    
    # Start the GUI
    try:
        root.mainloop()
    except KeyboardInterrupt:
        print("\nApplication interrupted by user")
    except Exception as e:
        print(f"Application error: {e}")
    finally:
        try:
            root.destroy()
        except:
            pass

if __name__ == "__main__":
    main()